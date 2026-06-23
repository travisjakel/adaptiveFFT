#' Confusion matrix counts
#'
#' @param y Integer vector of binary truth labels (0/1).
#' @param pred Integer vector of binary predictions (0/1).
#' @return A `list` with named integer counts `hi`, `fa`, `mi`, `cr`
#'   (hits, false alarms, misses, correct rejections) and `n_dropped`
#'   (rows excluded due to NA).
#' @export
#' @examples
#' confusion(c(1,1,0,0), c(1,0,1,0))
confusion <- function(y, pred) {
  ok <- !is.na(pred) & !is.na(y)
  y <- y[ok]; pred <- pred[ok]
  hi <- sum(pred == 1L & y == 1L)
  fa <- sum(pred == 1L & y == 0L)
  mi <- sum(pred == 0L & y == 1L)
  cr <- sum(pred == 0L & y == 0L)
  list(hi = hi, fa = fa, mi = mi, cr = cr, n_dropped = sum(!ok))
}

#' Accuracy
#'
#' @inheritParams confusion
#' @return Scalar accuracy.
#' @export
m_acc <- function(y, pred) mean(pred == y, na.rm = TRUE)

#' Balanced accuracy
#'
#' Returns `(sens + spec) / 2`. Returns `NA` if either class is empty
#' in the truth vector.
#'
#' @inheritParams confusion
#' @return Scalar balanced accuracy in `[0, 1]`.
#' @export
m_bacc <- function(y, pred) {
  c <- confusion(y, pred)
  sens <- if ((c$hi + c$mi) == 0) NA_real_ else c$hi / (c$hi + c$mi)
  spec <- if ((c$cr + c$fa) == 0) NA_real_ else c$cr / (c$cr + c$fa)
  (sens + spec) / 2
}

#' F1 score
#'
#' @inheritParams confusion
#' @return Scalar F1.
#' @export
m_f1 <- function(y, pred) {
  c <- confusion(y, pred)
  prec <- if ((c$hi + c$fa) == 0) NA_real_ else c$hi / (c$hi + c$fa)
  rec  <- if ((c$hi + c$mi) == 0) NA_real_ else c$hi / (c$hi + c$mi)
  if (is.na(prec) || is.na(rec) || (prec + rec) == 0) return(NA_real_)
  2 * prec * rec / (prec + rec)
}

#' Area under the ROC curve (rank-based / Mann-Whitney)
#'
#' @param y Integer vector of binary truth labels (0/1).
#' @param score Numeric vector of class-1 scores (higher = more positive).
#' @return Scalar AUC. `NA` if `score` is `NULL` or `y` is single-class.
#' @export
m_auc <- function(y, score) {
  if (is.null(score)) return(NA_real_)
  if (length(unique(y)) < 2L) return(NA_real_)
  r  <- rank(score)
  n1 <- sum(y == 1L); n0 <- sum(y == 0L)
  (sum(r[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

#' Mean cues used per prediction (frugality)
#'
#' @param n_cues_per_row Integer vector — depth at which each test case
#'   exited the tree.
#' @return Scalar mean cues per row, or `NA` if input is `NULL`.
#' @export
m_frugality <- function(n_cues_per_row) {
  if (is.null(n_cues_per_row)) return(NA_real_)
  mean(n_cues_per_row, na.rm = TRUE)
}

#' Compute all metrics on a fitted-result list
#'
#' Convenience wrapper that runs the per-metric helpers on a result list
#' produced by [adaptive_fft()] / [run_beam_fft()] / any other runner
#' returning the standard contract `(pred, score?, n_cues_per_row?, train_time)`.
#'
#' @param result List with elements `y_test`, `pred`, optional `score`,
#'   optional `n_cues_per_row`, and optional `train_time`.
#' @return Named list with `acc`, `bacc`, `f1`, `auc`, `frugality`,
#'   `train_time`, `n_test`, `n_pos_test`.
#' @export
#' @examples
#' set.seed(1)
#' y <- rbinom(50, 1, 0.4)
#' p <- ifelse(runif(50) < 0.8, y, 1L - y)
#' score_all(list(y_test = y, pred = p, n_cues_per_row = sample(1:4, 50, TRUE),
#'                train_time = 0.01))
score_all <- function(result) {
  y    <- result$y_test
  pred <- result$pred
  list(
    acc        = m_acc(y, pred),
    bacc       = m_bacc(y, pred),
    f1         = m_f1(y, pred),
    auc        = m_auc(y, result$score),
    frugality  = m_frugality(result$n_cues_per_row),
    train_time = result$train_time,
    n_test     = length(y),
    n_pos_test = sum(y == 1L)
  )
}
