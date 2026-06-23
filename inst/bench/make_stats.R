## Dataset-level statistical analysis (per gemini review round 1) ----------
## Aggregates each (dataset × method) to one seed-mean bacc, then runs:
##   1. Wilcoxon signed-rank on 14 dataset-level paired means (correct inference)
##   2. Nadeau-Bengio corrected-resampled t-test (steelman)
##   3. Rank-biserial correlation effect size
##   4. Holm-Bonferroni adjusted p-values across per-method comparisons

suppressPackageStartupMessages({
  library(data.table); library(fst)
})
OUT <- "_FFT_improve/paper/tables"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
dt <- as.data.table(read_fst("_FFT_improve/results/baseline_bench.fst"))

## Methods to test against ifan baseline
reference <- "ifan"
contenders <- c("adaptive","adaptive_fast","dfan","doe09_L4_T5_B10_C50",
                "rpart","beam","streed4","streed4_bacc","streed_tuned_bacc",
                "root4","root8")
contenders <- intersect(contenders, unique(dt$runner))

## --- 1. Dataset-level means ------------------------------------------------
ds_means <- dt[seed_idx <= 20 & runner %in% c(reference, contenders),
               .(bacc = mean(bacc, na.rm = TRUE)),
               by = .(runner, dataset)]
ds_wide <- dcast(ds_means, dataset ~ runner, value.var = "bacc")
setnames(ds_wide, reference, "ifan_bacc")

## --- 2. Rank-biserial correlation for paired Wilcoxon ---------------------
## r_rb = 1 - 2·U / (n(n+1)/2)  where U = rank-sum of negative differences
rank_biserial <- function(x, y) {
  d <- x - y; d <- d[d != 0]
  if (length(d) < 2L) return(NA_real_)
  r <- rank(abs(d))
  w_pos <- sum(r[d > 0]); w_neg <- sum(r[d < 0])
  (w_pos - w_neg) / (w_pos + w_neg)
}

## --- 3. Nadeau-Bengio corrected-resampled t-test --------------------------
## Accounts for dependence between k resampling splits: var_corrected =
## (1/k + n_test/n_train) * s^2(deltas). We use k = 20 seeds, train_frac = 0.7.
nadeau_bengio_p <- function(deltas, train_frac = 0.70) {
  k <- length(deltas)
  if (k < 2L) return(NA_real_)
  m <- mean(deltas); s2 <- var(deltas)
  factor <- 1/k + (1 - train_frac) / train_frac
  se <- sqrt(factor * s2)
  if (se == 0) return(ifelse(m == 0, 1, 0))
  t_stat <- m / se
  2 * pt(-abs(t_stat), df = k - 1L)
}

## --- 4. Build the comparison table ----------------------------------------
cmp_rows <- list()
for (cont in contenders) {
  if (!cont %in% names(ds_wide)) next
  x <- ds_wide[[cont]]       # contender per-dataset mean
  y <- ds_wide$ifan_bacc     # reference per-dataset mean
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 3L) next
  x_ok <- x[ok]; y_ok <- y[ok]
  n <- length(x_ok)

  ## Dataset-level Wilcoxon (the correct one)
  w <- tryCatch(wilcox.test(x_ok, y_ok, paired = TRUE, exact = FALSE),
                error = function(e) list(p.value = NA))
  ## NB test on per-seed deltas pooled across datasets
  ## We pass the 20 × 14 seed-level deltas as the resampling vector
  seed_deltas <- dt[seed_idx <= 20 & runner %in% c(cont, reference),
                    .(bacc, runner, dataset, seed_idx)]
  seed_w <- dcast(seed_deltas, dataset + seed_idx ~ runner, value.var = "bacc")
  seed_w[, delta := get(cont) - get(reference)]
  nb_p <- nadeau_bengio_p(seed_w$delta[!is.na(seed_w$delta)])

  cmp_rows[[length(cmp_rows) + 1L]] <- data.table(
    method            = cont,
    n_datasets        = n,
    mean_delta        = round(mean(x_ok - y_ok), 4),
    median_delta      = round(median(x_ok - y_ok), 4),
    wins              = sum(x_ok > y_ok),
    losses            = sum(x_ok < y_ok),
    wilcoxon_p_dsLvl  = round(w$p.value, 4),
    rank_biserial     = round(rank_biserial(x_ok, y_ok), 3),
    nadeau_bengio_p   = round(nb_p, 4)
  )
}
cmp <- rbindlist(cmp_rows)
## Holm–Bonferroni correction across the 9 comparisons
cmp[, wilcoxon_p_holm   := round(p.adjust(wilcoxon_p_dsLvl, method = "holm"), 4)]
cmp[, nadeau_bengio_holm := round(p.adjust(nadeau_bengio_p,  method = "holm"), 4)]
cmp <- cmp[order(-mean_delta)]

cat("=== Dataset-level paired tests vs ifan (n = 14 paired means) ===\n")
print(cmp)

fwrite(cmp, file.path(OUT, "table2b_dataset_level_vs_ifan.csv"))

## --- Markdown table for paper --------------------------------------------
md <- c(
  "### Method vs ifan: dataset-level (n = 14 paired means) + Nadeau-Bengio (seeds).",
  "",
  "| method | Δ bacc | n_wins | Wilcoxon p (ds-level) | Holm | rank-biserial | NB p (seeds) | NB Holm |",
  "| :- | :-: | :-: | :-: | :-: | :-: | :-: | :-: |"
)
for (i in seq_len(nrow(cmp))) {
  r <- cmp[i]
  md <- c(md, sprintf(
    "| %s | %+.3f | %d / %d | %.3f | %.3f | %+.3f | %.3f | %.3f |",
    r$method, r$mean_delta, r$wins, r$losses,
    r$wilcoxon_p_dsLvl, r$wilcoxon_p_holm, r$rank_biserial,
    r$nadeau_bengio_p, r$nadeau_bengio_holm))
}
writeLines(md, file.path(OUT, "table2b_dataset_level_vs_ifan.md"))

## --- vs adaptive (for SOTA comparison) ------------------------------------
cmp2_rows <- list()
for (cont in c("streed4_bacc","streed_tuned_bacc","streed4","adaptive_fast")) {
  if (!cont %in% names(ds_wide) || cont == "adaptive") next
  x <- ds_wide[[cont]]; y <- ds_wide[["adaptive"]]
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 3L) next
  n <- sum(ok); x_ok <- x[ok]; y_ok <- y[ok]
  w <- tryCatch(wilcox.test(x_ok, y_ok, paired = TRUE, exact = FALSE),
                error = function(e) list(p.value = NA))
  seed_deltas <- dt[seed_idx <= 20 & runner %in% c(cont, "adaptive"),
                    .(bacc, runner, dataset, seed_idx)]
  seed_w <- dcast(seed_deltas, dataset + seed_idx ~ runner, value.var = "bacc")
  seed_w[, delta := get(cont) - get("adaptive")]
  nb_p <- nadeau_bengio_p(seed_w$delta[!is.na(seed_w$delta)])
  cmp2_rows[[length(cmp2_rows)+1L]] <- data.table(
    method = cont, n_datasets = n,
    mean_delta = round(mean(x_ok - y_ok), 4),
    wins = sum(x_ok > y_ok), losses = sum(x_ok < y_ok),
    wilcoxon_p_dsLvl = round(w$p.value, 4),
    rank_biserial = round(rank_biserial(x_ok, y_ok), 3),
    nadeau_bengio_p = round(nb_p, 4)
  )
}
cmp2 <- rbindlist(cmp2_rows)[order(-mean_delta)]
cat("\n=== vs adaptive ===\n"); print(cmp2)
fwrite(cmp2, file.path(OUT, "table2c_vs_adaptive.csv"))
