## run_full_bench.R -----------------------------------------------------
## Reproduces the 14 datasets x 20 seeds bench used in Jakel (2026).
## Single-core, FST checkpoint, ~3-4 hours start to finish.
##
## Usage:
##   Rscript inst/bench/run_full_bench.R          # from package source dir
## or
##   source(system.file("bench", "run_full_bench.R", package = "adaptiveFFT"))

suppressPackageStartupMessages({
  library(adaptiveFFT); library(data.table); library(fst); library(FFTrees)
})

OUT <- if (file.exists("inst/bench/results")) {
  "inst/bench/results/baseline_bench.fst"
} else {
  file.path(tempdir(), "baseline_bench.fst")
}
message("Writing results to: ", OUT)

## Single runner: just adaptive_fft. The full paper compares 11 contenders;
## see the project repository for ifan/dfan/STreeD/CART wrappers.
RUNNERS <- list(
  adaptive = function(tr, te) {
    f <- adaptive_fft(tr, te)
    list(pred = f$test_pred, n_cues_per_row = f$test_exits,
         train_time = NA_real_, notes = paste0("winner=", f$winner))
  }
)

read_results  <- function() if (file.exists(OUT)) as.data.table(read_fst(OUT)) else NULL
write_results <- function(dt) write_fst(dt, OUT, compress = 50)
log_msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

run_one <- function(dataset, seed_idx, runner_name) {
  dat <- load_fft_dataset(dataset)
  seed <- 2026L + 1000L * match(dataset, FFT_DATASETS) + seed_idx
  in_tr <- make_split(dat$y, seed)
  train <- dat[in_tr]; test <- dat[!in_tr]
  fn <- RUNNERS[[runner_name]]
  res <- tryCatch(fn(train, test),
                  error = function(e) list(error = conditionMessage(e)))
  if (!is.null(res$error)) {
    return(data.table(dataset = dataset, seed_idx = seed_idx, runner = runner_name,
                      acc = NA_real_, bacc = NA_real_, f1 = NA_real_, auc = NA_real_,
                      frugality = NA_real_, train_time = NA_real_,
                      n_test = nrow(test), n_pos_test = sum(test$y == 1L),
                      notes = paste("ERROR:", res$error)))
  }
  res$y_test <- test$y
  m <- score_all(res)
  data.table(dataset = dataset, seed_idx = seed_idx, runner = runner_name,
             acc = m$acc, bacc = m$bacc, f1 = m$f1, auc = m$auc,
             frugality = m$frugality, train_time = m$train_time,
             n_test = m$n_test, n_pos_test = m$n_pos_test,
             notes = if (is.null(res$notes)) "" else res$notes)
}

prev <- read_results()
combos <- CJ(dataset = FFT_DATASETS, seed_idx = 1:20, runner = names(RUNNERS),
             sorted = FALSE)
if (!is.null(prev)) {
  done <- prev[, .(dataset, seed_idx, runner, existing = TRUE)]
  combos <- done[combos, on = .(dataset, seed_idx, runner)][is.na(existing),
                 .(dataset, seed_idx, runner)]
}
log_msg(sprintf("pending %d combos", nrow(combos)))
if (nrow(combos) == 0L) { log_msg("nothing to do"); quit() }

acc <- list()
for (i in seq_len(nrow(combos))) {
  acc[[i]] <- run_one(combos$dataset[i], combos$seed_idx[i], combos$runner[i])
  if (i %% 20L == 0L) {
    log_msg(sprintf("%d / %d done", i, nrow(combos)))
    new_dt <- rbindlist(acc, fill = TRUE)
    write_results(rbindlist(list(prev, new_dt), fill = TRUE))
  }
}
final <- rbindlist(acc, fill = TRUE)
write_results(rbindlist(list(prev, final), fill = TRUE))
log_msg("done")
