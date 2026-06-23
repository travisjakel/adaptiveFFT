# Fit an adaptive chain Fast-and-Frugal Tree

Performs an inner 80/20 stratified holdout race between FFTrees' greedy
\`ifan\` algorithm and our beam-search-doe09 chain-FFT search. The
winning family is refit on the full training set and applied to the test
data.

## Usage

``` r
adaptive_fft(
  train,
  test,
  max_levels = 4L,
  inner_frac = 0.8,
  beam_B = 10L,
  beam_max_thr = 5L,
  beam_item_cap = 50L,
  baselines = NULL,
  dataset_name = "dataset"
)
```

## Arguments

- train, test:

  \`data.table\`s with binary outcome in column \`y\` (0/1 integers).

- max_levels:

  Maximum chain depth (1-4). Default 4.

- inner_frac:

  Fraction of training rows allocated to the inner train half during the
  family race. Default 0.80.

- beam_B, beam_max_thr, beam_item_cap:

  Hyperparameters passed to \[run_beam_fft()\]. Defaults match the
  D-optimal-DoE selected "beam-doe09" configuration: \`beam_B = 10\`,
  \`beam_max_thr = 5\`, \`beam_item_cap = 50\`.

- baselines:

  Optional named numeric list of baseline test-bacc values for the Δ
  panel of \[plot.adaptive_fft()\], e.g. \`list(ifan = 0.79,
  streed4_bacc = 0.81)\`. \`NULL\` omits the panel.

- dataset_name:

  Short label used in plot headers.

## Value

Object of class \`adaptive_fft\`. List with elements: \`dataset_name\`,
\`n_train\`, \`n_test\`, \`base_rate\`, \`winner\`, \`scores\`
(data.table of inner-valid bacc per family), \`chain\` (data.table of
the winning tree's nodes), \`test_pred\`, \`test_y\`, \`test_exits\`,
\`test_bacc\`, \`test_acc\`, \`test_f1\`, \`test_frug\`, \`baselines\`,
\`max_levels\`, \`inner_frac\`.

## Details

Reproduces the procedure described in Jakel (2026, §3.2 "adaptive"):
adaptive yields a +1.4 pp balanced-accuracy point estimate over \`ifan\`
across 14 datasets x 20 stratified seeds (rank-biserial r = 0.44), while
preserving strict chain-FFT structure (depth ≤ \`max_levels\`, one cue
per node, binary exits at every level).

## References

Jakel, T. (2026). \*Adaptive Fast-and-Frugal Trees: Model-Family
Selection Narrows the Gap to Optimal Sparse Trees on Imbalanced
Benchmarks.\* arXiv preprint.

## See also

\[print.adaptive_fft()\], \[summary.adaptive_fft()\],
\[plot.adaptive_fft()\] for the S3 methods. \[run_beam_fft()\] for the
standalone beam search.

## Examples

``` r
if (FALSE) { # \dontrun{
  d <- load_fft_dataset("heartdisease")
  in_tr <- make_split(d$y, seed = 2026 + 1001)
  fit <- adaptive_fft(d[in_tr], d[!in_tr], dataset_name = "heartdisease")
  print(fit)
  plot(fit)
} # }
```
