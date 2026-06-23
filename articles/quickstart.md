# Quickstart: fitting an adaptive FFT

`adaptiveFFT` fits chain Fast-and-Frugal Trees (depth ≤ 4, one cue per
node, binary exits) and additionally races two FFT families on an inner
holdout to pick the better one for each dataset.

## Load a dataset and split

``` r

library(adaptiveFFT)

d <- load_fft_dataset("heartdisease")
#> Warning in `[.data.table`(dat, , `:=`(y, y)): A shallow copy of this data.table
#> was taken so that := can add or remove 1 columns by reference. At an earlier
#> point, this data.table was copied by R (or was created manually using
#> structure() or similar). Avoid names<- and attr<- which in R currently (and
#> oddly) may copy the whole data.table. Use set* syntax instead to avoid copying:
#> ?set, ?setnames and ?setattr. It's also not unusual for data.table-agnostic
#> packages to produce tables affected by this issue. If this message doesn't
#> help, please report your use case to the data.table issue tracker so the root
#> cause can be fixed or this message improved.
in_tr <- make_split(d$y, seed = 2026L + 1001L)
tr <- d[in_tr]; te <- d[!in_tr]
nrow(tr); nrow(te)
#> [1] 211
#> [1] 92
```

## Fit and inspect

``` r

fit <- adaptive_fft(tr, te, dataset_name = "heartdisease",
                    baselines = list(`ifan (paper)` = 0.787))
print(fit)
#> Adaptive FFT — heartdisease
#>   n_train = 211, n_test = 92, base rate = 0.46
#>   Inner race: ifan=0.685  beam-doe09=0.729  → winner: beam-doe09
#>   Chain (depth 2): ca → oldpeak
#>   Test: bacc=0.657  acc=0.652  f1=0.652  frugality=1.61 cues
#>   Deltas vs baselines:
#>     vs ifan (paper)       -0.130
```

`fit$winner` is whichever family won the inner race; `fit$chain` shows
the predicate sequence of the winning tree.

## Plot

The default [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
renders a 4-panel figure mirroring FFTrees’ visual grammar — tree with
side icon arrays, confusion matrix, test metrics, and Δ-vs-baseline
plot:

``` r

plot(fit)
```

You can request individual panels:

``` r

plot(fit, what = "tree")
plot(fit, what = "perf")
```

Or save publication-ready output:

``` r

plot(fit, save_to = "fig_demo")  # writes fig_demo.{pdf,png}
```

## Standalone beam search

If you only want the chain-FFT search without the inner race:

``` r

r <- run_beam_fft(tr, te, max_levels = 4L, beam_B = 10L,
                  max_thresholds_per_cue = 5L, item_cap = 50L)
m_bacc(te$y, r$pred)
```

## Reproduce the paper’s benchmark

The bundled benchmark script reproduces the 14 datasets × 20 seeds grid:

``` r

system.file("bench", "run_full_bench.R", package = "adaptiveFFT")
```

See the `Benchmarking` vignette for details.
