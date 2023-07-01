
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidystats

<!-- badges: start -->


[![](https://www.r-pkg.org/badges/version/tidystats?color=green)](https://cran.r-project.org/package=tidystats) [![](http://cranlogs.r-pkg.org/badges/grand-total/tidystats?color=green)](https://cran.r-project.org/package=tidystats) [![](http://cranlogs.r-pkg.org/badges/last-month/tidystats?color=green)](https://cran.r-project.org/package=tidystats) [![](http://cranlogs.r-pkg.org/badges/last-week/tidystats?color=green)](https://cran.r-project.org/package=tidystats) [![](https://img.shields.io/badge/doi-10.5281/zenodo.4041859-blue.svg)](https://doi.org/10.5281/zenodo.4041859)
<!-- badges: end -->

**Author:** [Willem Sleegers](https://www.willemsleegers.com/)
**License:** [MIT](https://opensource.org/licenses/MIT)

tidystats is an R package for sharing and reporting statistics. tidystats 
extracts statistics from the output of statistical functions 
(e.g., `t.test()`, `lm()`) and stores them in a structured format. The resulting
file can be shared with others and used in popular text editors to reproducibly 
report the statistics.

Please see below for instructions on how to install and use this package. 

**Do note that the package is currently in development. This means the package 
may contain bugs and is subject to significant changes.** If you find any bugs 
or if you have any feedback, please let me know by creating an issue here on 
Github.

## Installation

tidystats can be installed from CRAN.


```r
install.packages("tidystats")
```

You can also install the development version from GitHub using the 
[`remotes`](https://github.com/r-lib/remotes) package.


```r
remotes::install_github("willemsleegers/tidystats")
```

## Usage

The main function is `add_stats()`. The function has 2 necessary arguments:

- `list`: A list you want to add the statistics to. 
- `output`: The output of a statistics function (e.g., the output of `t.test()` 
  or `lm()`)

You also need an identifier to uniquely identify the output of a statistics
function. You can provide an identifier (e.g., 'weight_height_correlation') with
the `identifier` argument. If you do not provide an identifer, one is 
automatically created for you.

Optionally, you can also specify some additional meta-information:

- `type`: A type that specifies the analysis as primary, secondary, or 
  exploratory.
- `preregistered`: Whether the analysis was preregistered or not.
- `notes`: Additional information you think is useful to record.

Once all statistics are added to the list, you can write the contents to a 
file using the `write_stats()` function.

## Example

The following example shows how to combine and save the statistics from three
different statistical tests.


```r
# Conduct a t-test, regression, and an ANOVA
sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)

ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
weight <- c(ctl, trt)
lm_D9 <- lm(weight ~ group)

npk_aov <- aov(yield ~ block + N * P * K, npk)

# Create an empty list to add the statistics to
statistics <- list()

# Add the statistics and specify some meta-information
statistics <- statistics |>
  add_stats(sleep_test, type = "primary") |>
  add_stats(lm_D9, preregistered = TRUE) |>
  add_stats(npk_aov, notes = "An ANOVA example")

# Save the statistics to a file
write_stats(statistics, "statistics.json")
```

The result is a .json file that contains all the statistics from the three 
statistical tests. If you want to see what this file looks like, you can inspect
it [here](https://github.com/WillemSleegers/tidystats/blob/master/tests/data/main.json).

For a fully worked out example, see `vignette("introduction-to-tidystats")`.

## Supported statistical functions

tidystats supports functions from several statistics-related packages, 
including stats, lme4, BayesFactor, emmeans, and others. For a full list of 
supported packages and their functions, see `vignette("supported-functions")`. 

In some cases you need provide a `class` to the `add_stats()` 
function in order for tidystats to correctly extract the statistics. You can 
see a list of functions that require the `class` argument in the documentation
of the `add_stats()` function (`?add_stats`).

If you want to use tidystats on an unsupported function, there are two things
you can do:

1. Request support for the new function by creating an 
   [issue](https://github.com/WillemSleegers/tidystats/issues).
2. Manually extract the statistics and add them via `add_stats()` using the 
   `custom_stats()` function. See the `vignette("custom-statistics")`
   for more information.

## Reporting statistics

The file created with the `write_stats()` function can be used in several text
editor add-ins to reproducibly report the statistics. For more information on 
these add-ins, please see the [tidystats website](https://www.tidystats.io) or 
their GitHub pages:

- [Microsoft Word add-in](https://github.com/WillemSleegers/tidystats-Word-add-in)
- [Google Docs add-in](https://github.com/WillemSleegers/tidystats-Google-Docs-add-in)

## More information

See the [tidystats website](https://www.tidystats.io) for more information, 
such as a FAQ, tips and tricks, as well as how to receive (and give) support. 

If you have any questions or comments, feel free to create an 
[issue](https://github.com/WillemSleegers/tidystats/issues) here on GitHub or 
see the website for ways to contact me.
