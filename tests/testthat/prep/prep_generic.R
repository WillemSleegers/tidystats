
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
results <- list()

# BIC/AIC -----------------------------------------------------------------

# Run analysis
generic_ICs <- list(
  name = "BIC/AIC",
  statistics = list(
    list(name = "BIC", value = 21.21),
    list(name = "AIC", value = 478.21)
  )
)

# Add stats
results <- add_stats(
  list = results, 
  output = generic_ICs, 
  notes = "Wagenmakers (2007) method for calculating Bayes factors"
)

# Inspect output
generic_ICs

# CI ----------------------------------------------------------------------

# Run analyses
fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
fit_CIs <- confint(fit)

generic_CI <- list(
  name = "disp confidence intervals",
  statistics = list(
    list(
      name = "estimate", 
      symbol = "b",
      interval = "CI",
      level = ".95",
      value = fit$coefficients["disp"],
      lower = fit_CIs[2, 1],
      upper = fit_CIs[2, 2]
    )
  )
)

# Add stats
results <- add_stats(
  list = results, 
  output = generic_CI, 
  notes = "CI of only the disp coefficient"
)

# Inspect output
generic_CI

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/generic.json")

# Cleanup -----------------------------------------------------------------

rm(generic_ICs, fit, fit_CIs, generic_CI, df, results)
