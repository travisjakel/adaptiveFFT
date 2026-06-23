# Balanced accuracy

Returns \`(sens + spec) / 2\`. Returns \`NA\` if either class is empty
in the truth vector.

## Usage

``` r
m_bacc(y, pred)
```

## Arguments

- y:

  Integer vector of binary truth labels (0/1).

- pred:

  Integer vector of binary predictions (0/1).

## Value

Scalar balanced accuracy in \`\[0, 1\]\`.
