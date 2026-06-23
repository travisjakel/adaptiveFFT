skip_on_cran()
skip_if_not_installed("FFTrees")

test_that("adaptive_fft fits and returns required structure on heartdisease", {
  d <- load_fft_dataset("heartdisease")
  in_tr <- make_split(d$y, seed = 2026L + 1001L)
  fit <- adaptive_fft(d[in_tr], d[!in_tr], dataset_name = "heartdisease",
                      beam_B = 5L, beam_max_thr = 3L)

  expect_s3_class(fit, "adaptive_fft")
  expect_true(fit$winner %in% c("ifan", "beam-doe09"))
  expect_gte(fit$test_bacc, 0.5)
  expect_named(fit$chain, c("level","cue","direction","threshold","exit"))
  expect_length(fit$test_pred, fit$n_test)
  expect_length(fit$test_y, fit$n_test)
  expect_length(fit$test_exits, fit$n_test)
})

test_that("plot.adaptive_fft returns a ggplot/patchwork object without saving", {
  d <- load_fft_dataset("blood")
  in_tr <- make_split(d$y, seed = 2026L + 2001L)
  fit <- adaptive_fft(d[in_tr], d[!in_tr], dataset_name = "blood",
                      beam_B = 5L, beam_max_thr = 3L)
  p <- plot(fit, what = "tree")
  expect_s3_class(p, c("gg","ggplot"), exact = FALSE)
})

test_that("run_beam_fft solo run produces predictions of correct length", {
  d <- load_fft_dataset("blood")
  in_tr <- make_split(d$y, seed = 2026L + 2002L)
  r <- run_beam_fft(d[in_tr], d[!in_tr], max_levels = 3L,
                    beam_B = 5L, max_thresholds_per_cue = 3L, item_cap = 30L)
  expect_length(r$pred, sum(!in_tr))
  expect_true(all(r$pred %in% c(0L, 1L)))
  expect_named(r$chain_def, c("level","cue","direction","threshold","exit"))
})
