
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
  add_stats(cohens_d) %>%
  add_stats(cohens_d_not_pooled) %>%
  add_stats(cohens_d_mu) %>%
  add_stats(cohens_d_less) %>% 
  add_stats(cohens_d_one_sample) %>%
  add_stats(cohens_d_paired)

# Inspect output
cohens_d
cohens_d_not_pooled
cohens_d_mu
cohens_d_less
cohens_d_one_sample
cohens_d_paired

# hedges_g() --------------------------------------------------------------

# Run analyses
hedges_g <- hedges_g(mpg ~ am, data = mtcars)
hedges_g_not_pooled <- hedges_g(mpg ~ am, data = mtcars, pooled_sd = FALSE)
hedges_g_mu <- hedges_g(mpg ~ am, data = mtcars, mu = -5)
hedges_g_less <- hedges_g(mpg ~ am, data = mtcars, alternative = "less")
hedges_g_one_sample <- hedges_g(wt ~ 1, data = mtcars)
hedges_g_paired <- hedges_g(Pair(extra[group == 1], extra[group == 2]) ~ 1, 
                            data = sleep)

# Add stats
statistics <- statistics %>%
  add_stats(hedges_g) %>%
  add_stats(hedges_g_not_pooled) %>%
  add_stats(hedges_g_mu) %>% 
  add_stats(hedges_g_less) %>%
  add_stats(hedges_g_one_sample) %>%
  add_stats(hedges_g_paired)

# Inspect output
hedges_g
hedges_g_not_pooled
hedges_g_mu
hedges_g_less
hedges_g_one_sample
hedges_g_paired

# glass_delta() -----------------------------------------------------------

# Run analyses
glass_delta <- glass_delta(mpg ~ am, data = mtcars)
glass_delta_mu <- glass_delta(mpg ~ am, data = mtcars, mu = -5) 
glass_delta_less <- glass_delta(mpg ~ am, data = mtcars, alternative = "less")

# Add stats
statistics <- statistics %>%
  add_stats(glass_delta) %>%
  add_stats(glass_delta_mu) %>%
  add_stats(glass_delta_less)

# Inspect output
glass_delta
glass_delta_mu
glass_delta_less

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/testthat/data/effectsize.json")

# Cleanup -----------------------------------------------------------------

rm(
  statistics, cohens_d, cohens_d_not_pooled, cohens_d_mu, cohens_d_less, 
  cohens_d_one_sample, cohens_d_paired, df
)

