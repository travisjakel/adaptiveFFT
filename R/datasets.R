#' @importFrom data.table data.table set as.data.table
NULL

## Magic numbers --------------------------------------------------------------
.adaptive_defaults <- list(
  N_SEEDS    = 20L,
  TRAIN_FRAC = 0.70,
  BASE_SEED  = 2026L
)

#' Names of the 14 FFTrees benchmark datasets
#'
#' These are the classification datasets bundled with the
#' \pkg{FFTrees} package (Phillips, Neth, Woike & Gaissmaier, 2017),
#' minus pre-split variants of `heartdisease` (`heart.train`, `heart.test`,
#' `heart.cost`).
#'
#' @export
FFT_DATASETS <- c(
  "blood", "breastcancer", "car", "contraceptive", "creditapproval",
  "fertility", "forestfires", "heartdisease", "iris.v", "mushrooms",
  "sonar", "titanic", "voting", "wine"
)

## Per-dataset rule for choosing the positive class.
.binarize_rules <- list(
  blood          = list(col = "donation.crit", pos = TRUE),
  breastcancer   = list(col = "diagnosis",     pos = TRUE),
  car            = list(col = "acceptability", pos = c("good","vgood")),
  contraceptive  = list(col = "cont.crit",     pos = TRUE),
  creditapproval = list(col = "crit",          pos = TRUE),
  fertility      = list(col = "diagnosis",     pos = TRUE),
  forestfires    = list(col = "fire.crit",     pos = TRUE),
  heartdisease   = list(col = "diagnosis",     pos = 1),
  iris.v         = list(col = "virginica",     pos = TRUE),
  mushrooms      = list(col = "poisonous",     pos = TRUE),
  sonar          = list(col = "mine.crit",     pos = TRUE),
  titanic        = list(col = "survived",      pos = 1),
  voting         = list(col = "party.crit",    pos = TRUE),
  wine           = list(col = "type",          pos = "red")
)

#' Load one of the 14 FFTrees benchmark datasets
#'
#' Returns a `data.table` whose binary criterion is named `y` (`integer`,
#' 0/1). Character columns are coerced to factors so downstream tree
#' fitters (rpart, C5.0, FFTrees) handle them natively. NA values are
#' preserved (FFTrees handles NAs internally).
#'
#' @param name One of the 14 names listed in [FFT_DATASETS].
#' @return `data.table` with the binary outcome in column `y`.
#' @seealso [make_split()] for the deterministic stratified 70/30 split.
#' @export
#' @examples
#' \donttest{
#'   d <- load_fft_dataset("heartdisease")
#'   table(d$y)
#' }
load_fft_dataset <- function(name) {
  rule <- .binarize_rules[[name]]
  if (is.null(rule)) stop("No binarize rule for ", name)
  e <- new.env(parent = emptyenv())
  utils::data(list = name, package = "FFTrees", envir = e)
  raw <- e[[name]]
  dat <- data.table::as.data.table(raw)
  y <- as.integer(dat[[rule$col]] %in% rule$pos)
  dat[[rule$col]] <- NULL
  char_cols <- names(dat)[vapply(dat, is.character, logical(1))]
  for (cc in char_cols) data.table::set(dat, j = cc, value = as.factor(dat[[cc]]))
  dat[, y := y]
  dat[]
}

#' Deterministic stratified 70/30 train/test split
#'
#' Maintains the per-class proportions of `y` in both halves. Reproducible
#' given `seed`.
#'
#' @param y Integer vector of binary labels.
#' @param seed Integer seed.
#' @param train_frac Fraction of cases assigned to train. Default 0.70 to
#'   match the benchmark.
#' @return Logical vector of length `length(y)`; `TRUE` = in train.
#' @export
#' @examples
#' set.seed(1)
#' y <- rbinom(100, 1, 0.3)
#' in_tr <- make_split(y, seed = 42)
#' table(in_tr, y)
make_split <- function(y, seed, train_frac = 0.70) {
  set.seed(seed)
  n   <- length(y)
  idx_pos <- which(y == 1L); idx_neg <- which(y == 0L)
  n_train_pos <- floor(length(idx_pos) * train_frac)
  n_train_neg <- floor(length(idx_neg) * train_frac)
  train_pos <- sample(idx_pos, n_train_pos)
  train_neg <- sample(idx_neg, n_train_neg)
  in_train <- logical(n)
  in_train[c(train_pos, train_neg)] <- TRUE
  in_train
}
