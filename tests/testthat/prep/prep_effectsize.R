
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(effectsize)

# Create an empty list
statistics <- list()

# cohens_d() --------------------------------------------------------------

# Run analyses
cohens_d <- cohens_d(mpg ~ am, data = mtcars)
cohens_d_not_pooled <- cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
cohens_d_mu <- cohens_d(mpg ~ am, data = mtcars, mu = -5)
cohens_d_less <- cohens_d(mpg ~ am, data = mtcars, alternative = "less")
cohens_d_one_sample <- cohens_d(wt ~ 1, data = mtcars)
cohens_d_paired <- cohens_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, 
  data = sleep)

# Add stats
statistics <- statistics %>%
  add_stats(cohens_d)

# Inspect output
cohens_d
cohens_d_not_pooled
cohens_d_mu
cohens_d_less
cohens_d_one_sample
cohens_d_paired

# hedges_g() --------------------------------------------------------------


# glass_delta() -----------------------------------------------------------

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/testthat/data/effectsize.json")

# Cleanup -----------------------------------------------------------------

rm(
  statistics, cohens_d, cohens_d_not_pooled, cohens_d_mu, cohens_d_less, 
  cohens_d_one_sample, cohens_d_paired, df
)

