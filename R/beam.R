#' @importFrom data.table data.table rbindlist set as.data.table .I :=
NULL

## Marginal normalized MI of a binary predicate vs. binary label.
## @noRd
.mi_norm_binary <- function(pred_tf, y_tf) {
  n <- length(pred_tf)
  if (n == 0L) return(0)
  pp <- c(mean(pred_tf & y_tf), mean(pred_tf & !y_tf),
          mean(!pred_tf & y_tf), mean(!pred_tf & !y_tf))
  pX <- c(mean(pred_tf), 1 - mean(pred_tf))
  pY <- c(mean(y_tf),    1 - mean(y_tf))
  if (any(pX == 0) || any(pY == 0)) return(0)
  ind <- c(pX[1]*pY[1], pX[1]*pY[2], pX[2]*pY[1], pX[2]*pY[2])
  keep <- pp > 0 & ind > 0
  if (!any(keep)) return(0)
  I <- sum(pp[keep] * log2(pp[keep] / ind[keep]))
  hY <- -sum(pY * log2(pY))
  if (hY == 0) 0 else I / hY
}

## Build per-cue predicate items.
## @noRd
.build_items <- function(train, y_tf, max_thresholds_per_cue = 20L) {
  feat <- setdiff(names(train), "y")
  rows <- vector("list", 0)
  for (cue in feat) {
    x <- train[[cue]]
    if (is.numeric(x)) {
      u  <- sort(unique(x[!is.na(x)]))
      if (length(u) < 2L) next
      th <- (u[-length(u)] + u[-1L]) / 2
      if (length(th) > max_thresholds_per_cue) {
        mi_v <- vapply(th, function(t) .mi_norm_binary(x > t, y_tf), numeric(1))
        keep <- order(-mi_v)[seq_len(max_thresholds_per_cue)]
        th   <- th[sort(keep)]
      }
      for (t in th) for (dir in c(">", "<=")) {
        dec <- if (dir == ">") x > t else x <= t
        dec[is.na(dec)] <- FALSE
        rows[[length(rows) + 1L]] <- data.table::data.table(
          cue = cue, direction = dir, threshold = t, decision = list(dec))
      }
    } else if (is.logical(x)) {
      dec <- x; dec[is.na(dec)] <- FALSE
      rows[[length(rows) + 1L]] <- data.table::data.table(
        cue = cue, direction = "=", threshold = "TRUE",  decision = list(dec))
      rows[[length(rows) + 1L]] <- data.table::data.table(
        cue = cue, direction = "=", threshold = "FALSE", decision = list(!dec))
    } else if (is.factor(x) || is.character(x)) {
      lv <- levels(as.factor(x))
      for (l in lv) {
        dec <- as.character(x) == l
        dec[is.na(dec)] <- FALSE
        rows[[length(rows) + 1L]] <- data.table::data.table(
          cue = cue, direction = "=", threshold = as.character(l),
          decision = list(dec))
      }
    }
  }
  items <- data.table::rbindlist(rows)
  items[, item_id := .I]
  items[]
}

## @noRd
.eval_chain <- function(dec_mat, order_items, exits_nt, terminal_exit) {
  n <- nrow(dec_mat)
  pred  <- rep(NA_integer_, n)
  level <- rep(NA_integer_, n)
  active <- rep(TRUE, n)
  d <- length(order_items)
  if (d >= 2L) {
    for (i in seq_len(d - 1L)) {
      dec <- dec_mat[, order_items[i]]
      if (exits_nt[i] == 1L) { stopped <- active & dec;  pred[stopped] <- 1L }
      else                   { stopped <- active & !dec; pred[stopped] <- 0L }
      level[stopped] <- i
      active <- active & !stopped
    }
  }
  dec <- dec_mat[, order_items[d]]
  pred[active & dec]   <- as.integer(terminal_exit)
  pred[active & !dec]  <- as.integer(1L - terminal_exit)
  level[active]        <- d
  list(pred = pred, level = level)
}

## @noRd
.bacc_int <- function(y, pred) {
  hi <- sum(pred == 1L & y == 1L); mi <- sum(pred == 0L & y == 1L)
  cr <- sum(pred == 0L & y == 0L); fa <- sum(pred == 1L & y == 0L)
  sens <- if ((hi+mi)==0) 0.5 else hi/(hi+mi)
  spec <- if ((cr+fa)==0) 0.5 else cr/(cr+fa)
  (sens + spec) / 2
}

## @noRd
.beam_search <- function(dec_mat, y, max_levels, beam_B = 50L,
                         allow_repeat_cue = FALSE, items_meta) {
  n_items <- ncol(dec_mat)
  y_int <- as.integer(y)
  score_chain <- function(order_items, exits_nt, terminal_exit) {
    ev <- .eval_chain(dec_mat, order_items, exits_nt, terminal_exit)
    .bacc_int(y_int, ev$pred)
  }
  cand <- vector("list", 2L * n_items); k <- 1L
  for (i in seq_len(n_items)) for (te in c(0L, 1L)) {
    cand[[k]] <- list(order_items = i, exits_nt = integer(0),
                      terminal_exit = te,
                      bacc = score_chain(i, integer(0), te),
                      cues_used = items_meta$cue[i])
    k <- k + 1L
  }
  cand <- cand[order(-vapply(cand, `[[`, 0, "bacc"))][seq_len(min(beam_B, length(cand)))]
  best <- cand[[1]]
  if (max_levels == 1L) return(best)
  for (depth in 2:max_levels) {
    next_cand <- vector("list", 0)
    for (prev in cand) {
      for (i in seq_len(n_items)) {
        if (!allow_repeat_cue && items_meta$cue[i] %in% prev$cues_used) next
        new_exits_nt <- c(prev$exits_nt, prev$terminal_exit)
        for (te in c(0L, 1L)) {
          ord <- c(prev$order_items, i)
          bacc <- score_chain(ord, new_exits_nt, te)
          next_cand[[length(next_cand)+1L]] <- list(
            order_items = ord, exits_nt = new_exits_nt, terminal_exit = te,
            bacc = bacc, cues_used = c(prev$cues_used, items_meta$cue[i]))
        }
      }
    }
    if (length(next_cand) == 0L) break
    next_cand <- next_cand[order(-vapply(next_cand, `[[`, 0, "bacc"))]
    cand <- next_cand[seq_len(min(beam_B, length(next_cand)))]
    if (cand[[1]]$bacc > best$bacc) best <- cand[[1]]
  }
  best
}

## @noRd
.marginal_bacc <- function(items, y_tf) {
  vapply(seq_len(nrow(items)), function(j) {
    pred <- as.logical(items$decision[[j]])
    hi <- sum(pred & y_tf); mi <- sum(!pred & y_tf)
    cr <- sum(!pred & !y_tf); fa <- sum(pred & !y_tf)
    sens <- if ((hi+mi)==0) 0.5 else hi/(hi+mi)
    spec <- if ((cr+fa)==0) 0.5 else cr/(cr+fa)
    (sens + spec) / 2
  }, numeric(1))
}

## @noRd
.beam_chain_def <- function(items, best) {
  ord <- best$order_items
  d <- length(ord)
  it <- items[ord]
  exits <- c(best$exits_nt, best$terminal_exit)
  data.table::data.table(
    level = seq_len(d),
    cue = it$cue,
    direction = it$direction,
    threshold = as.character(it$threshold),
    exit = as.numeric(exits)
  )
}

#' Beam-search chain Fast-and-Frugal Tree
#'
#' Searches the chain-FFT space (depth `max_levels`, one cue per node,
#' binary exits) by beam search over all (cue, direction, threshold,
#' exit-pattern) combinations. The search optimizes balanced accuracy on
#' the training set; output is a chain of depth at most `max_levels`.
#'
#' Hyperparameter defaults `max_levels = 4`, `beam_B = 10`,
#' `max_thresholds_per_cue = 5`, `item_cap = 50` correspond to the
#' D-optimal-DoE-selected configuration ("beam-doe09") used in the
#' accompanying paper.
#'
#' @param train,test `data.table`s with binary criterion in column `y`.
#' @param max_levels Maximum chain depth (1-4).
#' @param beam_B Beam width — number of partial chains carried forward
#'   between depth levels.
#' @param max_thresholds_per_cue For each numeric cue, the top `K`
#'   midpoint thresholds by marginal normalized MI are kept.
#' @param item_cap Maximum number of (cue, direction, threshold) items
#'   retained for the search; pre-ranked by marginal balanced accuracy.
#'   `Inf` keeps all.
#' @return List with the standard runner contract:
#'   * `pred` — integer vector of test predictions (0/1)
#'   * `n_cues_per_row` — integer vector of exit levels (1..d)
#'   * `train_time` — elapsed seconds
#'   * `chain_def` — `data.table` with columns `level, cue, direction, threshold, exit`
#'   * `notes` — diagnostic string
#' @seealso [adaptive_fft()] races this against FFTrees `ifan` on an
#'   inner holdout.
#' @export
#' @examples
#' \dontrun{
#'   d <- load_fft_dataset("heartdisease")
#'   in_tr <- make_split(d$y, seed = 42)
#'   r <- run_beam_fft(d[in_tr], d[!in_tr])
#'   m_bacc(d$y[!in_tr], r$pred)
#' }
run_beam_fft <- function(train, test, max_levels = 4L, beam_B = 10L,
                         max_thresholds_per_cue = 5L, item_cap = 50) {
  y_tf <- as.logical(train$y)
  items <- .build_items(train, y_tf, max_thresholds_per_cue)
  if (is.finite(item_cap) && nrow(items) > item_cap) {
    mb <- .marginal_bacc(items, y_tf)
    keep <- order(-mb)[seq_len(item_cap)]
    items <- items[sort(keep)]
    items[, item_id := .I]
  }
  n_tr <- nrow(train); n_te <- nrow(test)
  dec_tr <- do.call(cbind, lapply(items$decision, as.logical))
  dec_te <- matrix(FALSE, nrow = n_te, ncol = nrow(items))
  for (j in seq_len(nrow(items))) {
    cue <- items$cue[j]; x <- test[[cue]]
    d <- switch(items$direction[j],
      ">"   = x > as.numeric(items$threshold[j]),
      "<="  = x <= as.numeric(items$threshold[j]),
      "="   = as.character(x) == as.character(items$threshold[j]),
      rep(FALSE, n_te))
    d[is.na(d)] <- FALSE
    dec_te[, j] <- d
  }
  t0 <- Sys.time()
  best <- .beam_search(dec_tr, as.integer(train$y),
                       max_levels = max_levels, beam_B = beam_B,
                       allow_repeat_cue = FALSE, items_meta = items)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ev <- .eval_chain(dec_te, best$order_items, best$exits_nt, best$terminal_exit)
  chain_def <- .beam_chain_def(items, best)
  list(pred = ev$pred, score = NULL,
       n_cues_per_row = ev$level,
       train_time = elapsed,
       chain_def = chain_def,
       notes = sprintf("beam_B=%d_levels=%d_train_bacc=%.3f",
                       beam_B, length(best$order_items), best$bacc))
}
