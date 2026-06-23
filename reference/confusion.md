# Confusion matrix counts

Confusion matrix counts

## Usage

``` r
confusion(y, pred)
```

## Arguments

- y:

  Integer vector of binary truth labels (0/1).

- pred:

  Integer vector of binary predictions (0/1).

## Value

A \`list\` with named integer counts \`hi\`, \`fa\`, \`mi\`, \`cr\`
(hits, false alarms, misses, correct rejections) and \`n_dropped\` (rows
excluded due to NA).

## Examples

``` r
confusion(c(1,1,0,0), c(1,0,1,0))
#> $hi
#> [1] 1
#> 
#> $fa
#> [1] 1
#> 
#> $mi
#> [1] 1
#> 
#> $cr
#> [1] 1
#> 
#> $n_dropped
#> [1] 0
#> 
```
