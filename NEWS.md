# adaptiveFFT 0.1.0

* Initial public release accompanying the arXiv preprint
  "Adaptive Fast-and-Frugal Trees: Model-Family Selection Narrows
  the Gap to Optimal Sparse Trees on Imbalanced Benchmarks."
* Public API: `adaptive_fft()` constructor with `print()`, `summary()`,
  `plot()` S3 methods; `run_beam_fft()` standalone beam search;
  `score_all()` metric helper; bundled benchmark results.
* 14 dataset loaders mirroring the FFTrees benchmark suite.
* Reproducibility script `inst/bench/run_full_bench.R`.
