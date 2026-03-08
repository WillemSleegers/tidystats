# Calculate common descriptive statistics

`describe_data()` returns a set of common descriptive statistics (e.g.,
number of observations, mean, standard deviation) for one or more
numeric variables.

## Usage

``` r
describe_data(data, ..., by = NULL, na.rm = TRUE, short = FALSE)
```

## Arguments

- data:

  A data frame.

- ...:

  One or more unquoted column names from the data frame.

- by:

  An optional character vector of column names to group by.

- na.rm:

  A boolean indicating whether missing values (including NaN) should be
  excluded in calculating the descriptives? The default is TRUE.

- short:

  A boolean indicating whether only a subset of descriptives should be
  reported? If set to
  ``` TRUE``, only the N, M, and SD will be returned. The default is  ```FALSE\`.

## Details

Use the `by` argument to group the data, or alternatively pipe grouped
data created with
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Examples

``` r
describe_data(quote_source, response)
#>        var missing    N        M       SD         SE min max range median mode
#> 1 response      18 6325 5.588617 2.189027 0.02752458   1   9     8      5    5
#>         skew kurtosis
#> 1 -0.1371395 2.340773

describe_data(quote_source, response, na.rm = FALSE)
#>        var missing    N  M SD SE min max range median mode skew kurtosis
#> 1 response      18 6325 NA NA NA  NA  NA    NA     NA    5   NA       NA

describe_data(quote_source, response, by = "source")
#>        var     source missing    N        M       SD         SE min max range
#> 1 response  Bin Laden      18 3083 5.232241 2.112639 0.03804858   1   9     8
#> 2 response Washington       0 3242 5.927514 2.206828 0.03875806   1   9     8
#>   median mode       skew kurtosis
#> 1      5    5 -0.0781437 2.585193
#> 2      6    5 -0.2341109 2.180309

describe_data(quote_source, response, by = "source", short = TRUE)
#>        var     source    N        M       SD
#> 1 response  Bin Laden 3083 5.232241 2.112639
#> 2 response Washington 3242 5.927514 2.206828
```
