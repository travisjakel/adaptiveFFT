#' adaptiveFFT: Adaptive Fast-and-Frugal Trees
#'
#' Chain Fast-and-Frugal Trees (FFTs) of depth at most four — one cue per
#' node, binary exits — fit by either FFTrees' greedy `ifan` algorithm or
#' a beam search over the chain space. The [adaptive_fft()] constructor
#' performs an inner-holdout race between the two families and refits the
#' winner on the full training set.
#'
#' @section Main entry points:
#' * [adaptive_fft()] — constructor returning an `adaptive_fft` S3 object.
#' * [run_beam_fft()] — standalone beam search.
#' * [score_all()] — compute bacc / acc / f1 / auc / frugality.
#' * [load_fft_dataset()] — load one of 14 FFTrees benchmark datasets.
#'
#' @section S3 methods on `adaptive_fft`:
#' * [print.adaptive_fft()]
#' * [summary.adaptive_fft()]
#' * [plot.adaptive_fft()]
#'
#' @section Reproducing the benchmark:
#' See `inst/bench/run_full_bench.R` and the `benchmark` vignette.
#'
#' @keywords internal
#' @aliases adaptiveFFT-package
"_PACKAGE"
