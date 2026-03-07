# Convert a tidystats list to a data frame

`tidy_stats_to_data_frame()` converts a tidystats list to a data frame,
which can then be used to extract specific statistics using standard
subsetting functions (e.g.,
[`subset()`](https://rdrr.io/r/base/subset.html)).

## Usage

``` r
tidy_stats_to_data_frame(x)
```

## Arguments

- x:

  A tidystats list.

## Examples

``` r
# Conduct analyses
sleep_test <- t.test(
  sleep$extra[sleep$group == 1],
  sleep$extra[sleep$group == 2],
  paired = TRUE
)

ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
weight <- c(ctl, trt)
lm_D9 <- lm(weight ~ group)

npk_aov <- aov(yield ~ block + N * P * K, npk)

# Create an empty list to store the statistics in
statistics <- list()

# Add statistics
statistics <- statistics |>
  add_stats(sleep_test, type = "primary", preregistered = TRUE) |>
  add_stats(lm_D9) |>
  add_stats(npk_aov, notes = "An ANOVA example")

# Convert the list to a data frame
df <- tidy_stats_to_data_frame(statistics)

# Select all the p-values
subset(df, statistic_name == "p")
#>              identifier
#> sleep_test.5 sleep_test
#> lm_D9.6           lm_D9
#> lm_D9.12          lm_D9
#> lm_D9.17          lm_D9
#> npk_aov.6       npk_aov
#> npk_aov.12      npk_aov
#> npk_aov.18      npk_aov
#> npk_aov.24      npk_aov
#> npk_aov.30      npk_aov
#> npk_aov.36      npk_aov
#> npk_aov.42      npk_aov
#>                                                                analysis_name
#> sleep_test.5 sleep$extra[sleep$group == 1] and sleep$extra[sleep$group == 2]
#> lm_D9.6                                                       weight ~ group
#> lm_D9.12                                                      weight ~ group
#> lm_D9.17                                                      weight ~ group
#> npk_aov.6                                          yield ~ block + N * P * K
#> npk_aov.12                                         yield ~ block + N * P * K
#> npk_aov.18                                         yield ~ block + N * P * K
#> npk_aov.24                                         yield ~ block + N * P * K
#> npk_aov.30                                         yield ~ block + N * P * K
#> npk_aov.36                                         yield ~ block + N * P * K
#> npk_aov.42                                         yield ~ block + N * P * K
#>              group_name_1 group_name_2 statistic_name symbol subscript lower
#> sleep_test.5         <NA>         <NA>              p   <NA>      <NA>    NA
#> lm_D9.6             Model         <NA>              p   <NA>      <NA>    NA
#> lm_D9.12     Coefficients  (Intercept)              p   <NA>      <NA>    NA
#> lm_D9.17     Coefficients     groupTrt              p   <NA>      <NA>    NA
#> npk_aov.6           Terms        block              p   <NA>      <NA>    NA
#> npk_aov.12          Terms            N              p   <NA>      <NA>    NA
#> npk_aov.18          Terms            P              p   <NA>      <NA>    NA
#> npk_aov.24          Terms            K              p   <NA>      <NA>    NA
#> npk_aov.30          Terms          N:P              p   <NA>      <NA>    NA
#> npk_aov.36          Terms          N:K              p   <NA>      <NA>    NA
#> npk_aov.42          Terms          P:K              p   <NA>      <NA>    NA
#>                     value upper interval level
#> sleep_test.5 2.832890e-03    NA     <NA>    NA
#> lm_D9.6      2.490232e-01    NA     <NA>    NA
#> lm_D9.12     9.547128e-15    NA     <NA>    NA
#> lm_D9.17     2.490232e-01    NA     <NA>    NA
#> npk_aov.6    1.593879e-02    NA     <NA>    NA
#> npk_aov.12   4.371812e-03    NA     <NA>    NA
#> npk_aov.18   4.749041e-01    NA     <NA>    NA
#> npk_aov.24   2.879505e-02    NA     <NA>    NA
#> npk_aov.30   2.631653e-01    NA     <NA>    NA
#> npk_aov.36   1.686479e-01    NA     <NA>    NA
#> npk_aov.42   8.627521e-01    NA     <NA>    NA
```
