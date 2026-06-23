# Deterministic stratified 70/30 train/test split

Maintains the per-class proportions of \`y\` in both halves.
Reproducible given \`seed\`.

## Usage

``` r
make_split(y, seed, train_frac = 0.7)
```

## Arguments

- y:

  Integer vector of binary labels.

- seed:

  Integer seed.

- train_frac:

  Fraction of cases assigned to train. Default 0.70 to match the
  benchmark.

## Value

Logical vector of length \`length(y)\`; \`TRUE\` = in train.

## Examples

``` r
set.seed(1)
y <- rbinom(100, 1, 0.3)
in_tr <- make_split(y, seed = 42)
table(in_tr, y)
#>        y
#> in_tr    0  1
#>   FALSE 21 10
#>   TRUE  47 22
```
