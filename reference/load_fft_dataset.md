# Load one of the 14 FFTrees benchmark datasets

Returns a \`data.table\` whose binary criterion is named \`y\`
(\`integer\`, 0/1). Character columns are coerced to factors so
downstream tree fitters (rpart, C5.0, FFTrees) handle them natively. NA
values are preserved (FFTrees handles NAs internally).

## Usage

``` r
load_fft_dataset(name)
```

## Arguments

- name:

  One of the 14 names listed in \[FFT_DATASETS\].

## Value

\`data.table\` with the binary outcome in column \`y\`.

## See also

\[make_split()\] for the deterministic stratified 70/30 split.

## Examples

``` r
# \donttest{
  d <- load_fft_dataset("heartdisease")
#> Warning: A shallow copy of this data.table was taken so that := can add or remove 1 columns by reference. At an earlier point, this data.table was copied by R (or was created manually using structure() or similar). Avoid names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. It's also not unusual for data.table-agnostic packages to produce tables affected by this issue. If this message doesn't help, please report your use case to the data.table issue tracker so the root cause can be fixed or this message improved.
  table(d$y)
#> 
#>   0   1 
#> 164 139 
# }
```
