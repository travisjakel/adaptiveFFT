# Plot method for adaptive_fft

Renders the publication-quality 4-panel layout described in Jakel (2026,
fig 9): a 3-column flanking tree (Predict 'False' / chain / Predict
'True') with FFTrees-style icon arrays at each exit, plus a bottom row
of confusion matrix, test metrics, and Δ-vs-baseline segment plot.

## Usage

``` r
# S3 method for class 'adaptive_fft'
plot(
  x,
  what = c("all", "race", "tree", "perf"),
  main = NULL,
  save_to = NULL,
  width = 11,
  height = 10,
  ...
)
```

## Arguments

- x:

  Object of class \`adaptive_fft\`.

- what:

  Which panels to render. One of \`"all"\` (default), \`"race"\`
  (inner-holdout bar only), \`"tree"\` (tree panel only), or \`"perf"\`
  (bottom row only).

- main:

  Optional override for the plot title.

- save_to:

  Optional output stem (without extension). If supplied, the plot is
  saved as \`\<stem\>.pdf\` (cairo_pdf) and \`\<stem\>.png\` (300dpi).

- width, height:

  Figure dimensions in inches when saving. Defaults 11×10 work for the
  full 4-panel layout.

- ...:

  Ignored.

## Value

The composed \`ggplot\`/\`patchwork\` object, invisibly.

## Details

Visual grammar follows the \*Modern Minimal\* design variant: outlined
circle = negative class (False), outlined triangle = positive class
(True); filled = correct, hollow = wrong; teal/orange colorblind-safe
palette.
