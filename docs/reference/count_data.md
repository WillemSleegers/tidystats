# Count the number of observations

`count_data()` returns the number and proportion of observations for
categorical variables.

## Usage

``` r
count_data(data, ..., by = NULL, na.rm = FALSE, pct = FALSE)
```

## Arguments

- data:

  A data frame.

- ...:

  One or more unquoted (categorical) column names from the data frame,
  separated by commas.

- by:

  An optional character vector of column names to group by.

- na.rm:

  A boolean specifying whether missing values (including NaN) should be
  removed.

- pct:

  A boolean indicating whether to calculate percentages instead of
  proportions. The default is `FALSE`.

## Details

Use the `by` argument to group the data, or alternatively pipe grouped
data created with
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Examples

``` r
count_data(quote_source, source)
#> # A tibble: 2 × 3
#>   source         n  prop
#>   <chr>      <int> <dbl>
#> 1 Bin Laden   3101 0.489
#> 2 Washington  3242 0.511
count_data(quote_source, source, sex)
#> # A tibble: 6 × 4
#>   source     sex        n     prop
#>   <chr>      <chr>  <int>    <dbl>
#> 1 Bin Laden  female  2067 0.326   
#> 2 Washington female  2206 0.348   
#> 3 Bin Laden  male    1029 0.162   
#> 4 Washington male    1031 0.163   
#> 5 Washington NA         5 0.000788
#> 6 Bin Laden  NA         5 0.000788
count_data(quote_source, source, sex, na.rm = TRUE)
#> # A tibble: 4 × 4
#>   source     sex        n  prop
#>   <chr>      <chr>  <int> <dbl>
#> 1 Bin Laden  female  2067 0.326
#> 2 Washington female  2206 0.348
#> 3 Bin Laden  male    1029 0.162
#> 4 Washington male    1031 0.163
count_data(quote_source, source, sex, na.rm = TRUE, pct = TRUE)
#> # A tibble: 4 × 4
#>   source     sex        n   pct
#>   <chr>      <chr>  <int> <dbl>
#> 1 Bin Laden  female  2067  32.6
#> 2 Washington female  2206  34.8
#> 3 Bin Laden  male    1029  16.2
#> 4 Washington male    1031  16.3

# Use the by argument to calculate proportions within a group
count_data(quote_source, sex, by = "source")
#> # A tibble: 6 × 4
#>   source     sex        n    prop
#>   <chr>      <chr>  <int>   <dbl>
#> 1 Bin Laden  female  2067 0.667  
#> 2 Washington female  2206 0.680  
#> 3 Bin Laden  male    1029 0.332  
#> 4 Washington male    1031 0.318  
#> 5 Washington NA         5 0.00154
#> 6 Bin Laden  NA         5 0.00161
```
