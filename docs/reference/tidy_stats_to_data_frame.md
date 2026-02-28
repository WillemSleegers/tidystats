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
sleep_wide <- reshape(
  sleep,
  direction = "wide",
  idvar = "ID",
  timevar = "group",
  sep = "_"
)
sleep_test <- t.test(sleep_wide$extra_1, sleep_wide$extra_2, paired = TRUE)

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
#>              identifier                             analysis_name group_name_1
#> sleep_test.5 sleep_test sleep_wide$extra_1 and sleep_wide$extra_2         <NA>
#> lm_D9.6           lm_D9                            weight ~ group        Model
#> lm_D9.12          lm_D9                            weight ~ group Coefficients
#> lm_D9.17          lm_D9                            weight ~ group Coefficients
#> npk_aov.6       npk_aov                 yield ~ block + N * P * K        Terms
#> npk_aov.12      npk_aov                 yield ~ block + N * P * K        Terms
#> npk_aov.18      npk_aov                 yield ~ block + N * P * K        Terms
#> npk_aov.24      npk_aov                 yield ~ block + N * P * K        Terms
#> npk_aov.30      npk_aov                 yield ~ block + N * P * K        Terms
#> npk_aov.36      npk_aov                 yield ~ block + N * P * K        Terms
#> npk_aov.42      npk_aov                 yield ~ block + N * P * K        Terms
#>              group_name_2 statistic_name symbol subscript lower        value
#> sleep_test.5         <NA>              p   <NA>      <NA>    NA 2.832890e-03
#> lm_D9.6              <NA>              p   <NA>      <NA>    NA 2.490232e-01
#> lm_D9.12      (Intercept)              p   <NA>      <NA>    NA 9.547128e-15
#> lm_D9.17         groupTrt              p   <NA>      <NA>    NA 2.490232e-01
#> npk_aov.6           block              p   <NA>      <NA>    NA 1.593879e-02
#> npk_aov.12              N              p   <NA>      <NA>    NA 4.371812e-03
#> npk_aov.18              P              p   <NA>      <NA>    NA 4.749041e-01
#> npk_aov.24              K              p   <NA>      <NA>    NA 2.879505e-02
#> npk_aov.30            N:P              p   <NA>      <NA>    NA 2.631653e-01
#> npk_aov.36            N:K              p   <NA>      <NA>    NA 1.686479e-01
#> npk_aov.42            P:K              p   <NA>      <NA>    NA 8.627521e-01
#>              upper interval level
#> sleep_test.5    NA     <NA>    NA
#> lm_D9.6         NA     <NA>    NA
#> lm_D9.12        NA     <NA>    NA
#> lm_D9.17        NA     <NA>    NA
#> npk_aov.6       NA     <NA>    NA
#> npk_aov.12      NA     <NA>    NA
#> npk_aov.18      NA     <NA>    NA
#> npk_aov.24      NA     <NA>    NA
#> npk_aov.30      NA     <NA>    NA
#> npk_aov.36      NA     <NA>    NA
#> npk_aov.42      NA     <NA>    NA
```
