# Add statistical output to a tidystats list

`add_stats()` is used to add the output of a statistical test to a
tidystats list.

## Usage

``` r
add_stats(
  list,
  output,
  identifier = NULL,
  type = NULL,
  preregistered = NULL,
  notes = NULL,
  args = NULL,
  class = NULL
)
```

## Arguments

- list:

  A tidystats list.

- output:

  Output of a statistical test.

- identifier:

  A string identifying the model. Automatically created if not provided.

- type:

  A string specifying the type of analysis: primary, secondary, or
  exploratory.

- preregistered:

  A boolean specifying whether the analysis was preregistered or not.

- notes:

  A string specifying additional information.

- args:

  A list of additional arguments to customize which statistics should be
  extracted. See 'Details' for a list of supported functions and their
  arguments.

- class:

  A string to manually specify the class of the object so that tidystats
  knows how to extract the statistics. See 'Details' for a list of
  classes that are supported.

## Details

Many functions to perform statistical tests (e.g.,
[`t.test()`](https://rdrr.io/r/stats/t.test.html),
[`lm()`](https://rdrr.io/r/stats/lm.html)) return an object containing
the statistics. These objects can be stored in variables and used with
`add_stats()` to extract the statistics and add them to a list.

The list can be saved to a file using the
[`write_stats()`](https://willemsleegers.github.io/tidystats/reference/write_stats.md)
function.

For a list of supported functions, see
[`vignette("supported-functions", package = "tidystats")`](https://willemsleegers.github.io/tidystats/articles/supported-functions.md).

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
lm_D9_confint <- confint(lm_D9)

npk_aov <- aov(yield ~ block + N * P * K, npk)

# Create an empty list to store the statistics in
statistics <- list()

# Add statistics to the list
statistics <- statistics |>
  add_stats(sleep_test, type = "primary", preregistered = TRUE) |>
  add_stats(lm_D9) |>
  add_stats(lm_D9_confint, class = "confint") |>
  add_stats(npk_aov, notes = "An ANOVA example")
```
