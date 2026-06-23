# adaptiveFFT

<!-- badges: start -->
[![R-CMD-check](https://github.com/travisjakel/adaptiveFFT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/travisjakel/adaptiveFFT/actions/workflows/R-CMD-check.yaml)
[![License: GPL-3](https://img.shields.io/badge/License-GPL_3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![arXiv](https://img.shields.io/badge/arXiv-XXXX.XXXXX-b31b1b.svg)](https://arxiv.org/abs/XXXX.XXXXX)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXX)
<!-- badges: end -->

**Adaptive Fast-and-Frugal Trees** — chain decision trees of depth ≤ 4
with one cue per node and binary exits, fit by either FFTrees' greedy
`ifan` algorithm or by a beam search over the chain space (with
hyperparameters selected via D-optimal Design of Experiments). The
`adaptive_fft()` constructor performs an **inner-holdout race** between
the two families and refits the winner on the full training set.

> Companion code for **Jakel (2026)**, *Adaptive Fast-and-Frugal Trees:
> Model-Family Selection Narrows the Gap to Optimal Sparse Trees on
> Imbalanced Benchmarks* (arXiv preprint).
>
> Travis Jakel — [Asset Flow Capital](https://assetflow.ai).

## Headline result

| Method | Mean balanced accuracy (14 datasets × 20 seeds) | Δ vs `ifan` | n datasets where method wins |
|---|---|---|---|
| STreeD (cost-sensitive, NeurIPS 2023) | 0.801 | +0.018 | 8/14 |
| **`adaptive` (this package)** | **0.797** | **+0.014** | **6/14** |
| FFTrees `ifan` | 0.783 | — | — |
| ROOT (JASA 2025, depth 8) | 0.708 | −0.075 | 1/14 |

`adaptive` lands within 0.3 pp of the cost-sensitive STreeD baseline while
preserving strict chain-FFT structure (depth ≤ 4, one cue per node, binary
exits), and it does so without breaking the FFT interpretability contract.

## Install

```r
# install.packages("remotes")
remotes::install_github("travisjakel/adaptiveFFT")
```

## Quickstart

```r
library(adaptiveFFT)

d <- load_fft_dataset("heartdisease")
in_tr <- make_split(d$y, seed = 2026 + 1001)

fit <- adaptive_fft(d[in_tr], d[!in_tr], dataset_name = "heartdisease")
print(fit)
plot(fit)                      # full 4-panel publication figure
plot(fit, what = "tree")       # tree panel only
plot(fit, save_to = "fig9")    # writes fig9.{pdf,png}
```

See `vignette("quickstart")` for the full walkthrough and
`vignette("benchmark")` to reproduce the paper's leaderboard.

## What the constructor does

1. **Inner stratified 80/20 split** of the training data.
2. Fits two families on the inner-train half:
   - `ifan` (FFTrees greedy)
   - `beam-doe09` (beam search; B = 10, 5 thresholds/cue, 50-item cap)
3. Scores each on the inner-valid half (balanced accuracy).
4. **Refits the winner on the full training set** and predicts the
   provided test split.
5. Returns an `adaptive_fft` S3 object with `print`, `summary`, and
   `plot` methods.

## Citing

If you use this package, please cite both:

```r
citation("adaptiveFFT")
```

```bibtex
@article{jakel2026adaptive,
  title  = {Adaptive Fast-and-Frugal Trees: Model-Family Selection Narrows the Gap to Optimal Sparse Trees on Imbalanced Benchmarks},
  author = {Jakel, Travis},
  year   = {2026},
  journal = {arXiv preprint arXiv:XXXX.XXXXX}
}
@misc{adaptiveFFT-pkg,
  title        = {{adaptiveFFT}: Adaptive Fast-and-Frugal Trees with Inner-Holdout Family Selection},
  author       = {Jakel, Travis},
  organization = {Asset Flow Capital},
  year         = {2026},
  doi          = {10.5281/zenodo.XXXXXXX},
  url          = {https://github.com/travisjakel/adaptiveFFT}
}
```

## License

GPL-3.0-or-later. Built on top of the GPL-2 [FFTrees](https://CRAN.R-project.org/package=FFTrees)
package by Phillips, Neth, Woike & Gaissmaier.

---

Maintained by [Travis Jakel](https://github.com/travisjakel) at
[Asset Flow Capital](https://assetflow.ai).
