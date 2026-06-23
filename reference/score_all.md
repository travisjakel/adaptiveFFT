# Compute all metrics on a fitted-result list

Convenience wrapper that runs the per-metric helpers on a result list
produced by \[adaptive_fft()\] / \[run_beam_fft()\] / any other runner
returning the standard contract \`(pred, score?, n_cues_per_row?,
train_time)\`.

## Usage

``` r
score_all(result)
```

## Arguments

- result:

  List with elements \`y_test\`, \`pred\`, optional \`score\`, optional
  \`n_cues_per_row\`, and optional \`train_time\`.

## Value

Named list with \`acc\`, \`bacc\`, \`f1\`, \`auc\`, \`frugality\`,
\`train_time\`, \`n_test\`, \`n_pos_test\`.

## Examples

``` r
set.seed(1)
y <- rbinom(50, 1, 0.4)
p <- ifelse(runif(50) < 0.8, y, 1L - y)
score_all(list(y_test = y, pred = p, n_cues_per_row = sample(1:4, 50, TRUE),
               train_time = 0.01))
#> $acc
#> [1] 0.82
#> 
#> $bacc
#> [1] 0.8268921
#> 
#> $f1
#> [1] 0.8235294
#> 
#> $auc
#> [1] NA
#> 
#> $frugality
#> [1] 2.58
#> 
#> $train_time
#> [1] 0.01
#> 
#> $n_test
#> [1] 50
#> 
#> $n_pos_test
#> [1] 23
#> 
```
