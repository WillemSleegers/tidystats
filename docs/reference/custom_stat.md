# Create a custom statistic

`custom_stat()` is used together with the
[`custom_stats()`](https://willemsleegers.github.io/tidystats/reference/custom_stats.md)
function to add statistics from unsupported functions via
[`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md).
See the
[`custom_stats()`](https://willemsleegers.github.io/tidystats/reference/custom_stats.md)
function for more information.

## Usage

``` r
custom_stat(
  name,
  value,
  symbol = NULL,
  subscript = NULL,
  interval = NULL,
  level = NULL,
  lower = NULL,
  upper = NULL
)
```

## Arguments

- name:

  A string specifying the name of the statistic.

- value:

  The numeric value of the statistic.

- symbol:

  A string specifying the symbol of the statistic to use when reporting
  the statistic.

- subscript:

  A string specifying a subscript to use when reporting the statistic.

- interval:

  A string specifying the type of interval if the statistic is a ranged
  statistic (e.g., 95% confidence interval)

- level:

  A numeric value between 0 and 1 indicating the level of the interval.

- lower:

  The numeric value of the lower bound of the statistic.

- upper:

  The numeric value of the upper bound of the statistic.

## Examples

``` r
# Example 1: A single mean value
sample <- rnorm(1000, mean = 0, sd = 1)
mean <- mean(sample)

custom_stat(name = "mean", value = mean, symbol = "M")
#> [[1]]
#> $name
#> [1] "mean"
#> 
#> $value
#> [1] 0.007990182
#> 
#> $symbol
#> [1] "M"
#> 
#> attr(,"class")
#> [1] "tidystats" "list"     
#> 

# Example 2: A mean with a 95% confidence interval
sample <- rnorm(1000, mean = 0, sd = 1)
mean <- mean(sample)
se <- sd(sample) / sqrt(length(sample))
CI <- c(mean - 1.96 * se, mean + 1.96 * se)

custom_stat(
  name = "mean",
  value = mean,
  symbol = "M",
  interval = "CI",
  level = .95,
  lower = CI[1],
  upper = CI[2]
)
#> [[1]]
#> $name
#> [1] "mean"
#> 
#> $value
#> [1] -0.0170397
#> 
#> $symbol
#> [1] "M"
#> 
#> $interval
#> [1] "CI"
#> 
#> $level
#> [1] 0.95
#> 
#> $lower
#> [1] -0.07699647
#> 
#> $upper
#> [1] 0.04291707
#> 
#> attr(,"class")
#> [1] "tidystats" "list"     
#> 
```
