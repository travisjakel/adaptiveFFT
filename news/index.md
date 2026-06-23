# Changelog

## adaptiveFFT 0.1.0

- Initial public release accompanying the arXiv preprint “Adaptive
  Fast-and-Frugal Trees: Model-Family Selection Narrows the Gap to
  Optimal Sparse Trees on Imbalanced Benchmarks.”
- Public API:
  [`adaptive_fft()`](https://travisjakel.github.io/adaptiveFFT/reference/adaptive_fft.md)
  constructor with [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) S3 methods;
  [`run_beam_fft()`](https://travisjakel.github.io/adaptiveFFT/reference/run_beam_fft.md)
  standalone beam search;
  [`score_all()`](https://travisjakel.github.io/adaptiveFFT/reference/score_all.md)
  metric helper; bundled benchmark results.
- 14 dataset loaders mirroring the FFTrees benchmark suite.
- Reproducibility script `inst/bench/run_full_bench.R`.
