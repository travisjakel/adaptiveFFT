# Area under the ROC curve (rank-based / Mann-Whitney)

Area under the ROC curve (rank-based / Mann-Whitney)

## Usage

``` r
m_auc(y, score)
```

## Arguments

- y:

  Integer vector of binary truth labels (0/1).

- score:

  Numeric vector of class-1 scores (higher = more positive).

## Value

Scalar AUC. \`NA\` if \`score\` is \`NULL\` or \`y\` is single-class.
