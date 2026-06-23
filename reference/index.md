# Package index

## Adaptive constructor

- [`adaptive_fft()`](https://travisjakel.github.io/adaptiveFFT/reference/adaptive_fft.md)
  : Fit an adaptive chain Fast-and-Frugal Tree
- [`print(`*`<adaptive_fft>`*`)`](https://travisjakel.github.io/adaptiveFFT/reference/print.adaptive_fft.md)
  : Print method for adaptive_fft
- [`summary(`*`<adaptive_fft>`*`)`](https://travisjakel.github.io/adaptiveFFT/reference/summary.adaptive_fft.md)
  : Summary method for adaptive_fft
- [`plot(`*`<adaptive_fft>`*`)`](https://travisjakel.github.io/adaptiveFFT/reference/plot.adaptive_fft.md)
  : Plot method for adaptive_fft

## Standalone beam search

- [`run_beam_fft()`](https://travisjakel.github.io/adaptiveFFT/reference/run_beam_fft.md)
  : Beam-search chain Fast-and-Frugal Tree

## Datasets and splits

- [`FFT_DATASETS`](https://travisjakel.github.io/adaptiveFFT/reference/FFT_DATASETS.md)
  : Names of the 14 FFTrees benchmark datasets
- [`load_fft_dataset()`](https://travisjakel.github.io/adaptiveFFT/reference/load_fft_dataset.md)
  : Load one of the 14 FFTrees benchmark datasets
- [`make_split()`](https://travisjakel.github.io/adaptiveFFT/reference/make_split.md)
  : Deterministic stratified 70/30 train/test split

## Metrics

- [`score_all()`](https://travisjakel.github.io/adaptiveFFT/reference/score_all.md)
  : Compute all metrics on a fitted-result list
- [`m_bacc()`](https://travisjakel.github.io/adaptiveFFT/reference/m_bacc.md)
  : Balanced accuracy
- [`m_acc()`](https://travisjakel.github.io/adaptiveFFT/reference/m_acc.md)
  : Accuracy
- [`m_f1()`](https://travisjakel.github.io/adaptiveFFT/reference/m_f1.md)
  : F1 score
- [`m_auc()`](https://travisjakel.github.io/adaptiveFFT/reference/m_auc.md)
  : Area under the ROC curve (rank-based / Mann-Whitney)
- [`m_frugality()`](https://travisjakel.github.io/adaptiveFFT/reference/m_frugality.md)
  : Mean cues used per prediction (frugality)
- [`confusion()`](https://travisjakel.github.io/adaptiveFFT/reference/confusion.md)
  : Confusion matrix counts
