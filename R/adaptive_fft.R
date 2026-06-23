#' @import ggplot2
#' @import patchwork
#' @importFrom data.table data.table copy as.data.table is.data.table fifelse rbindlist :=
#' @importFrom scales percent_format
#' @importFrom grDevices cairo_pdf
#' @importFrom stats median reorder
NULL

## NULL-coalesce
`%||%` <- function(a, b) if (is.null(a)) b else a

## ------------------------------- palette / theme ----------------------
## Design-system tokens (Modern Minimal variant from the Interactive FFT
## handoff bundle — claude.ai/design RPwUcrbcucOdNCdQmqb_5w). Teal/orange
## colorblind-safe; warm off-white page; JetBrains Mono labels.
.dt <- list(
  healthy   = "#2A8A8E",   # teal — negative class (False)
  disease   = "#D17A2B",   # orange — positive class (True)
  text      = "#22222C",
  muted     = "#7A7A82",
  divider   = "#E2E0DA",
  panel     = "#FFFFFF",
  page      = "#F7F4ED",
  tint      = "#EEEAE0",
  accent    = "#FFE6BB",
  node_fill = "#FFFFFF"
)
## Legacy 9-slot palette retained for backward compat (legend dots etc).
.oi <- c("#22222C", "#E69F00", .dt$healthy, "#009E73", "#F0E442",
         "#0072B2", .dt$disease, "#CC79A7", "#7A7A82")

.theme_adaptive <- function() {
  theme_minimal(base_size = 11, base_family = "sans") +
    theme(
      plot.background = element_rect(fill = .dt$page, color = NA),
      panel.background = element_rect(fill = .dt$page, color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = .dt$divider, linewidth = 0.3),
      axis.line.x = element_line(color = .dt$muted),
      axis.line.y = element_line(color = .dt$muted),
      axis.text = element_text(color = .dt$text),
      strip.text = element_text(face = "bold", color = .dt$text),
      plot.title = element_text(face = "bold", size = 11.5, color = .dt$text),
      plot.subtitle = element_text(color = .dt$muted, size = 10),
      legend.position = "bottom", legend.title = element_blank()
    )
}

## ------------------------------- constructor -------------------------
#' Fit an adaptive chain Fast-and-Frugal Tree
#'
#' Performs an inner 80/20 stratified holdout race between FFTrees'
#' greedy `ifan` algorithm and our beam-search-doe09 chain-FFT search.
#' The winning family is refit on the full training set and applied
#' to the test data.
#'
#' Reproduces the procedure described in Jakel (2026, §3.2 "adaptive"):
#' adaptive yields a +1.4 pp balanced-accuracy point estimate over `ifan`
#' across 14 datasets x 20 stratified seeds (rank-biserial r = 0.44),
#' while preserving strict chain-FFT structure (depth ≤ `max_levels`,
#' one cue per node, binary exits at every level).
#'
#' @param train,test `data.table`s with binary outcome in column `y`
#'   (0/1 integers).
#' @param max_levels Maximum chain depth (1-4). Default 4.
#' @param inner_frac Fraction of training rows allocated to the inner
#'   train half during the family race. Default 0.80.
#' @param beam_B,beam_max_thr,beam_item_cap Hyperparameters passed to
#'   [run_beam_fft()]. Defaults match the D-optimal-DoE selected
#'   "beam-doe09" configuration: `beam_B = 10`, `beam_max_thr = 5`,
#'   `beam_item_cap = 50`.
#' @param baselines Optional named numeric list of baseline test-bacc
#'   values for the Δ panel of [plot.adaptive_fft()], e.g.
#'   `list(ifan = 0.79, streed4_bacc = 0.81)`. `NULL` omits the panel.
#' @param dataset_name Short label used in plot headers.
#' @return Object of class `adaptive_fft`. List with elements:
#'   `dataset_name`, `n_train`, `n_test`, `base_rate`, `winner`,
#'   `scores` (data.table of inner-valid bacc per family),
#'   `chain` (data.table of the winning tree's nodes),
#'   `test_pred`, `test_y`, `test_exits`, `test_bacc`, `test_acc`,
#'   `test_f1`, `test_frug`, `baselines`, `max_levels`, `inner_frac`.
#' @seealso [print.adaptive_fft()], [summary.adaptive_fft()],
#'   [plot.adaptive_fft()] for the S3 methods. [run_beam_fft()] for
#'   the standalone beam search.
#' @references
#' Jakel, T. (2026). *Adaptive Fast-and-Frugal Trees: Model-Family
#' Selection Narrows the Gap to Optimal Sparse Trees on Imbalanced
#' Benchmarks.* arXiv preprint.
#' @export
#' @examples
#' \dontrun{
#'   d <- load_fft_dataset("heartdisease")
#'   in_tr <- make_split(d$y, seed = 2026 + 1001)
#'   fit <- adaptive_fft(d[in_tr], d[!in_tr], dataset_name = "heartdisease")
#'   print(fit)
#'   plot(fit)
#' }
adaptive_fft <- function(train, test,
                         max_levels = 4L, inner_frac = 0.80,
                         beam_B = 10L, beam_max_thr = 5L, beam_item_cap = 50L,
                         baselines = NULL,
                         dataset_name = "dataset") {
  .require_runners()

  stopifnot(is.data.table(train) || is.data.frame(train))
  stopifnot(is.data.table(test)  || is.data.frame(test))
  train <- as.data.table(copy(train)); test <- as.data.table(copy(test))
  stopifnot("y" %in% names(train), "y" %in% names(test))

  n_tr <- nrow(train); n_te <- nrow(test)
  base_rate <- mean(as.integer(train$y) == 1L)

  ## Inner stratified 80/20
  idx_pos <- which(train$y == 1L); idx_neg <- which(train$y == 0L)
  in_tr <- sort(c(
    sample(idx_pos, floor(length(idx_pos) * inner_frac)),
    sample(idx_neg, floor(length(idx_neg) * inner_frac))
  ))
  in_va <- setdiff(seq_len(n_tr), in_tr)
  tr_in <- train[in_tr]; va_in <- train[in_va]

  ## Score each family on inner-valid
  r_ifan_inner <- .fit_fft(tr_in, va_in, algorithm = "ifan",
                           max.levels = max_levels, numthresh.n = 10L)
  bacc_ifan_inner <- m_bacc(va_in$y, r_ifan_inner$pred)

  r_beam_inner <- run_beam_fft(
    tr_in, va_in, max_levels = max_levels,
    beam_B = beam_B, max_thresholds_per_cue = beam_max_thr,
    item_cap = if (beam_item_cap >= 999L) Inf else beam_item_cap
  )
  bacc_beam_inner <- m_bacc(va_in$y, r_beam_inner$pred)

  winner <- if (bacc_beam_inner > bacc_ifan_inner) "beam-doe09" else "ifan"

  ## Refit winner on FULL train, predict test
  if (winner == "ifan") {
    r_final <- .fit_fft(train, test, algorithm = "ifan",
                        max.levels = max_levels, numthresh.n = 10L)
    chain <- .extract_chain_from_fftrees(r_final$fit_obj)
    test_pred <- r_final$pred
    test_frug <- mean(r_final$n_cues_per_row, na.rm = TRUE)
  } else {
    r_final <- run_beam_fft(
      train, test, max_levels = max_levels,
      beam_B = beam_B, max_thresholds_per_cue = beam_max_thr,
      item_cap = if (beam_item_cap >= 999L) Inf else beam_item_cap
    )
    chain <- r_final$chain_def %||% .extract_chain_from_beam(r_final)
    test_pred <- r_final$pred
    test_frug <- mean(r_final$n_cues_per_row, na.rm = TRUE)
  }

  test_y <- as.integer(test$y)
  test_pred <- as.integer(test_pred)
  test_exits <- as.integer(r_final$n_cues_per_row)

  scores <- data.table(
    family = c("ifan (FFTrees)", "beam-doe09 (ours)"),
    inner_bacc = c(bacc_ifan_inner, bacc_beam_inner),
    picked = c(winner == "ifan", winner == "beam-doe09")
  )

  out <- list(
    dataset_name    = dataset_name,
    n_train         = n_tr,
    n_test          = n_te,
    base_rate       = base_rate,
    winner          = winner,
    scores          = scores,
    chain           = chain,
    test_pred       = test_pred,
    test_y          = test_y,
    test_exits      = test_exits,
    test_bacc       = m_bacc(test_y, test_pred),
    test_acc        = m_acc(test_y, test_pred),
    test_f1         = m_f1(test_y, test_pred),
    test_frug       = test_frug,
    baselines       = baselines,
    max_levels      = max_levels,
    inner_frac      = inner_frac
  )
  class(out) <- "adaptive_fft"
  out
}

## ------------------------------- S3 methods -------------------------

#' Print method for adaptive_fft
#'
#' @param x Object of class `adaptive_fft`.
#' @param ... Ignored.
#' @return The object, invisibly.
#' @export
print.adaptive_fft <- function(x, ...) {
  cat(sprintf("Adaptive FFT — %s\n", x$dataset_name))
  cat(sprintf("  n_train = %d, n_test = %d, base rate = %.2f\n",
              x$n_train, x$n_test, x$base_rate))
  cat(sprintf("  Inner race: ifan=%.3f  beam-doe09=%.3f  → winner: %s\n",
              x$scores$inner_bacc[1], x$scores$inner_bacc[2], x$winner))
  cat(sprintf("  Chain (depth %d): %s\n",
              nrow(x$chain), paste(x$chain$cue, collapse = " → ")))
  cat(sprintf("  Test: bacc=%.3f  acc=%.3f  f1=%.3f  frugality=%.2f cues\n",
              x$test_bacc, x$test_acc, x$test_f1, x$test_frug))
  if (!is.null(x$baselines)) {
    cat("  Deltas vs baselines:\n")
    for (b in names(x$baselines)) {
      cat(sprintf("    vs %-18s %+.3f\n", b, x$test_bacc - x$baselines[[b]]))
    }
  }
  invisible(x)
}

#' Summary method for adaptive_fft
#'
#' Prints the same header as [print.adaptive_fft()] plus the full
#' `scores` (inner-race) and `chain` (winning tree) tables.
#'
#' @param object Object of class `adaptive_fft`.
#' @param ... Ignored.
#' @return The object, invisibly.
#' @export
summary.adaptive_fft <- function(object, ...) {
  print(object)
  cat("\nInner-holdout scores:\n"); print(object$scores)
  cat("\nChain nodes:\n"); print(object$chain)
  invisible(object)
}

## ------------------------------- panels ------------------------------

.build_icon_array <- function(dt, x_center, y_center) {
  if (nrow(dt) == 0) return(data.table(x = numeric(), y = numeric(),
                                       shape = numeric(), color = character(),
                                       tooltip = character()))
  d <- copy(dt)
  d[, is_correct := test_y == test_pred]
  d <- d[order(-is_correct, test_y)]
  N <- nrow(d)
  cols <- max(5, min(15, ceiling(sqrt(N) * 1.5)))
  spacing_x <- 1.0 / cols
  spacing_y <- 0.15

  d[, col := (seq_len(N) - 1) %% cols]
  d[, row := (seq_len(N) - 1) %/% cols]

  width <- (cols - 1) * spacing_x
  d[, x := x_center - width / 2 + col * spacing_x]
  d[, y := y_center - row * spacing_y]
  ## Design grammar: shape encodes class (circle = False/negative, triangle = True/positive),
  ## fill encodes correctness (filled = correct, hollow outline = wrong),
  ## color = the class color (teal negatives, orange positives).
  ## Solid shapes 16 (circle) / 17 (triangle); hollow 1 / 2.
  d[, shape := fifelse(test_y == 1L,
                       fifelse(is_correct, 17L, 2L),     # filled vs hollow triangle
                       fifelse(is_correct, 16L, 1L))]    # filled vs hollow circle
  d[, color := ifelse(test_y == 1, .dt$disease, .dt$healthy)]
  d[, outcome := fifelse(test_y == 1L & test_pred == 1L, "Hit",
                  fifelse(test_y == 0L & test_pred == 0L, "Correct Rejection",
                   fifelse(test_y == 1L & test_pred == 0L, "Miss",
                                                         "False Alarm")))]
  d[, tooltip := sprintf(
    "case #%d<br>truth: %s<br>pred: %s<br>outcome: <b>%s</b>",
    idx,
    ifelse(test_y == 1L, "True", "False"),
    ifelse(test_pred == 1L, "True", "False"),
    outcome)]
  d[, .(x, y, shape, color, tooltip)]
}

.panel_inner_race <- function(x, main = NULL) {
  d <- copy(x$scores)
  title <- main %||% "Inner Race Results"
  ggplot(d, aes(x = inner_bacc, y = reorder(family, inner_bacc), color = picked)) +
    geom_segment(aes(x = 0, xend = inner_bacc, y = family, yend = family),
                 linewidth = 1) +
    geom_point(size = 4) +
    geom_text(aes(label = sprintf("%.3f", inner_bacc), x = inner_bacc + 0.01),
              hjust = 0) +
    scale_color_manual(values = c("TRUE" = .oi[4], "FALSE" = .oi[9]),
                       guide = "none") +
    scale_x_continuous(limits = c(0, 1)) +
    labs(title = title, x = "Inner-valid bacc", y = NULL) +
    .theme_adaptive()
}

.invert_label <- function(direction, threshold) {
  ## Format: predicate "= n" → "≠ n", "> 0" → "≤ 0", etc.
  if (is.na(direction) || !nzchar(direction)) direction <- "="
  inv <- switch(direction,
    "="  = "≠",  "==" = "≠",
    "!=" = "=",
    ">"  = "≤",  ">=" = "<",
    "<"  = "≥",  "<=" = ">",
    direction)
  sprintf("%s %s", inv, threshold)
}

.fmt_label <- function(direction, threshold) {
  if (is.na(direction) || !nzchar(direction)) direction <- "="
  sprintf("%s %s", direction, threshold)
}

.panel_tree <- function(x, main = NULL) {
  chain <- x$chain; k <- nrow(chain)
  if (k == 0L) return(ggplot() + annotate("text", x = 0, y = 0,
                                          label = "(empty chain)") + theme_void())

  ## Layout coordinates -----------------------------------------------------
  ## Center column is the tree (x ≈ 0). Predict-False cluster sits far left,
  ## Predict-True cluster sits far right. Vertical spacing per level.
  V_STEP    <- 6.0                       # vertical distance between nodes
  TREE_X    <- 0
  EXIT_DX   <- 1.6                       # horizontal offset to exit circle
  ARRAY_X_F <- -5.0                      # left-column icon-array center
  ARRAY_X_T <-  5.0                      # right-column icon-array center
  node_y    <- seq(k, 1) * V_STEP        # top-down: level 1 at top

  pretty_thr <- function(s) {
    if (is.na(s)) return("")
    if (nchar(s) <= 12) return(s)
    parts <- strsplit(s, ",", fixed = TRUE)[[1]]
    if (length(parts) > 2L) return(sprintf("%s, …", trimws(parts[1])))
    paste0(substr(s, 1, 10), "…")
  }

  ## Test-case routing (per-level exit) -------------------------------------
  test_exits <- x$test_exits
  if (is.null(test_exits)) {
    test_exits <- rep(k, x$n_test)
    avail <- seq_len(x$n_test)
    for (i in seq_len(k - 1)) {
      idx <- avail[x$test_pred[avail] == chain$exit[i]]
      if (length(idx) > 0) {
        pick <- idx[seq_len(min(length(idx), max(1, length(idx) %/% 2)))]
        test_exits[pick] <- i
        avail <- setdiff(avail, pick)
      }
    }
  }
  dt_cases <- data.table(
    idx = seq_len(x$n_test),
    test_y = x$test_y, test_pred = x$test_pred, exit_node = test_exits)

  ## Geometry buffers --------------------------------------------------------
  arrows_list <- list(); exits_list <- list()
  icons_list  <- list(); counts_list <- list()
  side_lab    <- list(); down_lab    <- list()

  pred_label <- function(threshold, direction)
    .fmt_label(direction, pretty_thr(threshold))
  cont_label <- function(threshold, direction)
    .invert_label(direction, pretty_thr(threshold))

  for (i in seq_len(k)) {
    is_term <- (i == k)
    yn <- node_y[i]

    if (!is_term) {
      epred  <- chain$exit[i]                         # 1 = True / Disease, 0 = False / Healthy
      side   <- ifelse(epred == 1, 1, -1)
      ex_x   <- side * EXIT_DX
      arr_x  <- ifelse(epred == 1, ARRAY_X_T, ARRAY_X_F)

      ## (a) tree spine: vertical down to next node
      arrows_list[[length(arrows_list) + 1]] <- data.table(
        x = TREE_X, y = yn - 0.5, xend = TREE_X, yend = node_y[i + 1] + 0.5,
        kind = "spine")
      ## (b) side branch: horizontal to exit circle
      arrows_list[[length(arrows_list) + 1]] <- data.table(
        x = TREE_X + side * 0.7, y = yn,
        xend = ex_x - side * 0.25, yend = yn,
        kind = "exit")
      ## exit circle (H or D — using F/T for our generic binary)
      exits_list[[length(exits_list) + 1]] <- data.table(
        x = ex_x, y = yn, label = ifelse(epred == 1, "T", "F"),
        color = ifelse(epred == 1, .dt$disease, .dt$healthy))
      ## predicate label above side branch
      side_lab[[length(side_lab) + 1]] <- data.table(
        x = TREE_X + side * 0.95, y = yn + 0.5,
        label = pred_label(chain$threshold[i], chain$direction[i]),
        hjust = ifelse(side == 1, 0, 1))
      ## inverse predicate, italic, on the continue branch
      down_lab[[length(down_lab) + 1]] <- data.table(
        x = TREE_X - 1.0, y = (yn + node_y[i + 1]) / 2,
        label = cont_label(chain$threshold[i], chain$direction[i]),
        hjust = 1)

      ## icon array — cases that exited at this level (split by correctness)
      sub_cases <- dt_cases[exit_node == i & test_pred == epred]
      arr_y <- yn - 1.7
      ic_dt <- .build_icon_array(sub_cases, arr_x, arr_y)
      icons_list[[length(icons_list) + 1]] <- ic_dt

      if (nrow(ic_dt) > 0) {
        n_corr <- sum(sub_cases$test_y == sub_cases$test_pred)
        counts_list[[length(counts_list) + 1]] <- data.table(
          x = arr_x, y = min(ic_dt$y) - 0.5,
          label = sprintf("%d correct  |  %d wrong",
                          n_corr, nrow(sub_cases) - n_corr))
      } else {
        counts_list[[length(counts_list) + 1]] <- data.table(
          x = arr_x, y = arr_y - 0.5, label = "0 cases")
      }
    } else {
      ## Terminal node fans both ways
      for (epred in c(0, 1)) {
        side  <- ifelse(epred == 1, 1, -1)
        ex_x  <- side * EXIT_DX
        ex_y  <- yn - 1.6
        arr_x <- ifelse(epred == 1, ARRAY_X_T, ARRAY_X_F)
        arrows_list[[length(arrows_list) + 1]] <- data.table(
          x = TREE_X + side * 0.45, y = yn - 0.45,
          xend = ex_x - side * 0.2, yend = ex_y + 0.25,
          kind = "exit")
        exits_list[[length(exits_list) + 1]] <- data.table(
          x = ex_x, y = ex_y, label = ifelse(epred == 1, "T", "F"),
          color = ifelse(epred == 1, .dt$disease, .dt$healthy))
        ## terminal fan labels: predicate on the side, inverted on the other
        if (epred == 1) {
          side_lab[[length(side_lab) + 1]] <- data.table(
            x = TREE_X + 0.55, y = yn - 0.2,
            label = pred_label(chain$threshold[i], chain$direction[i]),
            hjust = 0)
        } else {
          side_lab[[length(side_lab) + 1]] <- data.table(
            x = TREE_X - 0.55, y = yn - 0.2,
            label = cont_label(chain$threshold[i], chain$direction[i]),
            hjust = 1)
        }
        sub_cases <- dt_cases[exit_node == i & test_pred == epred]
        arr_y <- ex_y - 1.4
        ic_dt <- .build_icon_array(sub_cases, arr_x, arr_y)
        icons_list[[length(icons_list) + 1]] <- ic_dt
        if (nrow(ic_dt) > 0) {
          n_corr <- sum(sub_cases$test_y == sub_cases$test_pred)
          counts_list[[length(counts_list) + 1]] <- data.table(
            x = arr_x, y = min(ic_dt$y) - 0.5,
            label = sprintf("%d correct  |  %d wrong",
                            n_corr, nrow(sub_cases) - n_corr))
        } else {
          counts_list[[length(counts_list) + 1]] <- data.table(
            x = arr_x, y = arr_y - 0.5, label = "0 cases")
        }
      }
    }
  }

  arrows_dt <- rbindlist(arrows_list)
  exits_dt  <- rbindlist(exits_list)
  icons_dt  <- rbindlist(icons_list)
  counts_dt <- rbindlist(counts_list)
  side_dt   <- rbindlist(side_lab)
  down_dt   <- rbindlist(down_lab)

  ## Header text -------------------------------------------------------------
  ymax <- max(node_y) + 3.0
  ymin <- min(counts_dt$y, min(node_y)) - 1.0
  family_label <- if (x$winner == "ifan") "ifan (FFTrees greedy)" else "beam-doe09 (chain-FFT search)"
  title_str <- main %||% sprintf("Adaptive FFT — %s", x$dataset_name)
  sub_str <- sprintf(
    "n = %d  |  base rate = %.0f%%  |  Inner race: %s (%.3f) beat %s (%.3f)",
    x$n_train + x$n_test, x$base_rate * 100,
    family_label, x$scores$inner_bacc[x$scores$picked],
    x$scores$family[!x$scores$picked], x$scores$inner_bacc[!x$scores$picked])

  ## Build plot --------------------------------------------------------------
  ggplot() +
    ## tree spine + side branches
    geom_segment(data = arrows_dt[kind == "spine"],
                 aes(x = x, y = y, xend = xend, yend = yend),
                 color = .dt$text, linewidth = 0.6,
                 arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
    geom_segment(data = arrows_dt[kind != "spine"],
                 aes(x = x, y = y, xend = xend, yend = yend),
                 color = .dt$text, linewidth = 0.5) +
    ## predicate labels (side, mono)
    geom_text(data = side_dt,
              aes(x = x, y = y, label = label, hjust = hjust),
              size = 3.2, color = .dt$text, family = "mono") +
    ## continue-branch inverted labels (italic)
    geom_text(data = down_dt,
              aes(x = x, y = y, label = label, hjust = hjust),
              size = 3.0, color = .dt$muted, family = "mono",
              fontface = "italic") +
    ## exit circles (small, hollow with letter)
    geom_point(data = exits_dt, aes(x = x, y = y),
               size = 7, shape = 21, fill = .dt$panel,
               color = .dt$text, stroke = 0.8) +
    geom_text(data = exits_dt,
              aes(x = x, y = y, label = label, color = color),
              size = 3.2, fontface = "bold", family = "mono") +
    ## decide-side labels next to exit circles
    geom_text(data = exits_dt,
              aes(x = x + ifelse(label == "T", 0.4, -0.4), y = y,
                  label = ifelse(label == "T", "True", "False"),
                  hjust = ifelse(label == "T", 0, 1)),
              size = 2.9, color = .dt$muted, fontface = "italic") +
    ## tree cue boxes (rounded white panels)
    geom_rect(data = data.table(y = node_y),
              aes(xmin = -0.7, xmax = 0.7, ymin = y - 0.5, ymax = y + 0.5),
              fill = .dt$node_fill, color = .dt$text, linewidth = 0.6) +
    geom_text(data = data.table(y = node_y, cue = chain$cue),
              aes(x = 0, y = y, label = cue),
              fontface = "bold", size = 4.2, color = .dt$text,
              family = "mono") +
    ## icon arrays — outlined glyphs colored by class
    geom_point(data = icons_dt,
               aes(x = x, y = y, shape = factor(shape),
                   color = color, text = tooltip),
               size = 2.6) +
    scale_shape_manual(values = c("1" = 1, "2" = 2, "16" = 16, "17" = 17),
                       guide = "none") +
    scale_color_identity() +
    ## per-cluster counts
    geom_text(data = counts_dt,
              aes(x = x, y = y, label = label),
              size = 3.0, color = .dt$muted, family = "mono") +
    ## column header strip — Predict 'False' / Predict 'True'
    annotate("text", x = ARRAY_X_F, y = ymax + 1.6,
             label = "Predict 'False'", fontface = "italic",
             color = .dt$text, size = 4.2) +
    annotate("text", x = ARRAY_X_T, y = ymax + 1.6,
             label = "Predict 'True'", fontface = "italic",
             color = .dt$text, size = 4.2) +
    ## legend dots — Correct Rejection / Miss / Hit / False Alarm
    annotate("point", x = ARRAY_X_F - 1.6, y = ymax + 0.5,
             shape = 16, color = .dt$healthy, size = 2.6) +
    annotate("text",  x = ARRAY_X_F - 1.4, y = ymax + 0.5,
             label = "Correct Rejection", hjust = 0, size = 2.8,
             color = .dt$muted, family = "mono") +
    annotate("point", x = ARRAY_X_F + 0.8, y = ymax + 0.5,
             shape = 17, color = .dt$disease, size = 2.6) +
    annotate("text",  x = ARRAY_X_F + 1.0, y = ymax + 0.5,
             label = "Miss", hjust = 0, size = 2.8,
             color = .dt$muted, family = "mono") +
    annotate("point", x = ARRAY_X_T - 1.6, y = ymax + 0.5,
             shape = 17, color = .dt$disease, size = 2.6) +
    annotate("text",  x = ARRAY_X_T - 1.4, y = ymax + 0.5,
             label = "Hit", hjust = 0, size = 2.8,
             color = .dt$muted, family = "mono") +
    annotate("point", x = ARRAY_X_T + 0.4, y = ymax + 0.5,
             shape = 16, color = .dt$healthy, size = 2.6) +
    annotate("text",  x = ARRAY_X_T + 0.6, y = ymax + 0.5,
             label = "False Alarm", hjust = 0, size = 2.8,
             color = .dt$muted, family = "mono") +
    coord_cartesian(xlim = c(ARRAY_X_F - 2.5, ARRAY_X_T + 2.5),
                    ylim = c(ymin, ymax + 2.4)) +
    labs(title = title_str, subtitle = sub_str) +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12,
                                    color = .dt$text),
          plot.subtitle = element_text(color = .dt$muted, size = 10),
          plot.background = element_rect(fill = .dt$page, color = NA),
          plot.margin = margin(10, 10, 10, 10))
}

.panel_confusion <- function(x) {
  c_ <- confusion(x$test_y, x$test_pred)
  df <- data.table(
    Truth = factor(c("True", "False", "True", "False"),
                   levels = c("False", "True")),
    Decision = factor(c("True", "True", "False", "False"),
                      levels = c("False", "True")),
    Count = c(c_$hi, c_$fa, c_$mi, c_$cr),
    Label = c("Hit", "False Alarm", "Miss", "Correct Rejection"),
    Color = c(.oi[4], .oi[7], .oi[7], .oi[4])
  )
  df[, tooltip := sprintf("%s<br>n = %d", Label, Count)]
  ggplot(df, aes(x = Truth, y = Decision, text = tooltip)) +
    geom_tile(fill = "white", color = "grey70", linewidth = 0.5) +
    geom_text(aes(label = Count), size = 6, fontface = "bold", vjust = -0.2) +
    geom_text(aes(label = Label, color = Color), vjust = 2.0, size = 3) +
    scale_color_identity() +
    labs(title = "Confusion Matrix", subtitle = "Test split",
         x = "Truth", y = "Decision") +
    .theme_adaptive() +
    theme(panel.grid = element_blank(), axis.line = element_blank())
}

.panel_metrics <- function(x) {
  c_ <- confusion(x$test_y, x$test_pred)
  sens <- if ((c_$hi + c_$mi) == 0) NA_real_ else c_$hi / (c_$hi + c_$mi)
  spec <- if ((c_$cr + c_$fa) == 0) NA_real_ else c_$cr / (c_$cr + c_$fa)
  df <- data.table(
    metric = factor(c("bacc", "sens", "spec", "acc"),
                    levels = c("bacc", "sens", "spec", "acc")),
    value = c(x$test_bacc, sens, spec, x$test_acc)
  )
  df[, tooltip := sprintf("%s = %.3f (%.1f%%)", metric, value, value * 100)]
  p1 <- ggplot(df, aes(x = metric, y = value, fill = metric, text = tooltip)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%", value * 100)),
              vjust = -0.5, size = 3.5) +
    scale_y_continuous(limits = c(0, 1.15), breaks = NULL) +
    scale_fill_manual(values = c(.oi[3], .oi[4], .oi[6], .oi[7]),
                      guide = "none") +
    labs(title = "Metrics", subtitle = "Test split", x = NULL, y = NULL) +
    .theme_adaptive()

  df_frug <- data.table(metric = "frugality", value = x$test_frug,
                        tooltip = sprintf("frugality = %.2f cues/case",
                                          x$test_frug))
  p2 <- ggplot(df_frug, aes(x = metric, y = value, text = tooltip)) +
    geom_col(width = 0.3, fill = .oi[9]) +
    geom_text(aes(label = sprintf("%.2f", value)), vjust = -0.5, size = 3.5) +
    scale_y_continuous(limits = c(0, x$max_levels * 1.1), breaks = NULL) +
    labs(title = "Frugality", subtitle = "Cues/case", x = NULL, y = NULL) +
    .theme_adaptive()

  p1 + p2 + plot_layout(widths = c(3, 1))
}

.panel_deltas <- function(x) {
  if (is.null(x$baselines) || length(x$baselines) == 0L) {
    return(ggplot() + theme_void())
  }
  contrasts <- data.table(
    comparison = names(x$baselines),
    delta = x$test_bacc - unlist(x$baselines)
  )
  contrasts[, sign := ifelse(delta >= 0, "gain", "loss")]
  contrasts[, label := sprintf("%+.3f", delta)]
  contrasts[, comparison := factor(comparison,
                                   levels = comparison[order(delta)])]
  contrasts[, tooltip := sprintf(
    "vs %s<br>adaptive %s by %.3f bacc",
    comparison, ifelse(delta >= 0, "wins", "loses"), abs(delta))]

  ggplot(contrasts, aes(x = delta, y = comparison, color = sign, text = tooltip)) +
    geom_vline(xintercept = 0, color = "grey50", linewidth = 0.6) +
    geom_segment(aes(x = 0, xend = delta, y = comparison, yend = comparison),
                 linewidth = 2.5) +
    geom_point(size = 5) +
    geom_text(aes(label = label,
                  x = delta + 0.015 * sign(delta + 1e-9)),
              size = 3.5) +
    scale_color_manual(values = c("gain" = .oi[4], "loss" = .oi[7]),
                       guide = "none") +
    scale_x_continuous(labels = scales::percent_format(1),
                       expand = expansion(mult = c(0.2, 0.2))) +
    labs(title = "\u0394 bacc vs baselines",
         subtitle = "Positive = adaptive wins",
         x = NULL, y = NULL) +
    .theme_adaptive()
}

#' Plot method for adaptive_fft
#'
#' Renders the publication-quality 4-panel layout described in Jakel
#' (2026, fig 9): a 3-column flanking tree (Predict 'False' / chain /
#' Predict 'True') with FFTrees-style icon arrays at each exit, plus a
#' bottom row of confusion matrix, test metrics, and Δ-vs-baseline
#' segment plot.
#'
#' Visual grammar follows the *Modern Minimal* design variant: outlined
#' circle = negative class (False), outlined triangle = positive class
#' (True); filled = correct, hollow = wrong; teal/orange colorblind-safe
#' palette.
#'
#' @param x Object of class `adaptive_fft`.
#' @param what Which panels to render. One of `"all"` (default),
#'   `"race"` (inner-holdout bar only), `"tree"` (tree panel only), or
#'   `"perf"` (bottom row only).
#' @param main Optional override for the plot title.
#' @param save_to Optional output stem (without extension). If supplied,
#'   the plot is saved as `<stem>.pdf` (cairo_pdf) and `<stem>.png` (300dpi).
#' @param width,height Figure dimensions in inches when saving. Defaults
#'   11×10 work for the full 4-panel layout.
#' @param ... Ignored.
#' @return The composed `ggplot`/`patchwork` object, invisibly.
#' @export
plot.adaptive_fft <- function(x,
                              what = c("all", "race", "tree", "perf"),
                              main = NULL,
                              save_to = NULL,
                              width = 11, height = 10,
                              ...) {
  what <- match.arg(what)

  plot_obj <- switch(what,
    all  = .panel_tree(x, main = main) /
           (.panel_confusion(x) | .panel_metrics(x) | .panel_deltas(x)) +
           plot_layout(heights = c(2.5, 1)),
    race = .panel_inner_race(x, main = main),
    tree = .panel_tree(x, main = main),
    perf = .panel_confusion(x) | .panel_metrics(x) | .panel_deltas(x)
  )

  if (!is.null(save_to)) {
    ggsave(paste0(save_to, ".pdf"), plot_obj, width = width, height = height,
           device = cairo_pdf)
    ggsave(paste0(save_to, ".png"), plot_obj, width = width, height = height,
           dpi = 300)
    message("Wrote ", save_to, ".{pdf,png}")
  }
  print(plot_obj)
  invisible(plot_obj)
}

## ------------------------------- helpers ------------------------------

## In-package: all dependencies are already loaded — no-op stub kept for
## backward compatibility with the dev-time autoloader.
## @noRd
.require_runners <- function() invisible(TRUE)

.extract_chain_from_fftrees <- function(fit) {
  best_i <- FFTrees::get_best_tree(fit, data = "train", goal = "bacc")
  def <- fit$trees$definitions[best_i, ]
  cues <- strsplit(def$cues, ";", fixed = TRUE)[[1]]
  dirs <- strsplit(def$directions, ";", fixed = TRUE)[[1]]
  thrs <- strsplit(def$thresholds, ";", fixed = TRUE)[[1]]
  exits<- as.numeric(strsplit(def$exits, ";", fixed = TRUE)[[1]])
  data.table(level = seq_along(cues), cue = cues,
             direction = dirs, threshold = thrs, exit = exits)
}

.extract_chain_from_beam <- function(r_beam) {
  ## run_beam_fft returns pred / n_cues_per_row / notes; the chain itself is
  ## the attribute we need. If run_beam_fft doesn't expose it, we decode from
  ## notes (contains "order=cue1:cue2:cue3"). Fallback: single-row placeholder.
  notes <- r_beam$notes %||% ""
  m <- regmatches(notes, regexec("order=([^_]+)", notes))[[1]]
  if (length(m) >= 2L) {
    cues <- strsplit(m[2], ":", fixed = TRUE)[[1]]
    return(data.table(level = seq_along(cues), cue = cues,
                      direction = NA_character_, threshold = NA_character_,
                      exit = NA_real_))
  }
  data.table(level = 1L, cue = "(beam chain — chain_def not exposed)",
             direction = NA_character_, threshold = NA_character_, exit = NA_real_)
}

## Make .fit_fft expose the fitted FFTrees object so we can extract the chain.
## (The existing run_ifan() discards it; adaptive_fft needs it.)
.fit_fft <- function(train, test, algorithm = c("ifan","dfan"),
                     max.levels = 4L, numthresh.n = 10L,
                     goal = "bacc") {
  algorithm <- match.arg(algorithm)
  tr2 <- copy(train); tr2[, y := as.logical(y)]
  te2 <- copy(test);  te2[,  y := as.logical(y)]
  t0  <- Sys.time()
  fit <- FFTrees::FFTrees(
    formula = y ~ ., data = as.data.frame(tr2),
    data.test = as.data.frame(te2), algorithm = algorithm,
    max.levels = max.levels, numthresh.n = numthresh.n,
    goal = goal, goal.chase = goal, goal.threshold = goal,
    quiet = list(ini=TRUE,fin=TRUE,mis=TRUE,set=TRUE), do.comp = FALSE
  )
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  best_i <- FFTrees::get_best_tree(fit, data = "train", goal = goal)
  dec <- fit$trees$decisions$test[[best_i]]
  list(
    pred = as.integer(dec$decision),
    n_cues_per_row = as.integer(dec$levelout),
    train_time = elapsed,
    fit_obj = fit,
    notes = sprintf("fft_%s_best=%d_levels=%d", algorithm, best_i, max.levels)
  )
}

