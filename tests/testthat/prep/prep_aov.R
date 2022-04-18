
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
results <- list()

# aov(): aov --------------------------------------------------------------

# Run tests
aov <- aov(yield ~ block + N * P * K, npk)
aov_order <- aov(terms(yield ~ block + N * P + K, keep.order = TRUE), npk)

# aov(): aovlist ----------------------------------------------------------

# Run test
aov_error <- aov(yield ~ N * P * K + Error(block), npk)

# add_stats() -------------------------------------------------------------

# Add stats
results <- results %>%
  add_stats(aov) %>%
  add_stats(aov_order) %>%
  add_stats(aov_error)

# Inspect output ----------------------------------------------------------

summary(aov)
summary(aov_order)
summary(aov_error)

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/aov.json")

# Cleanup -----------------------------------------------------------------

rm(aov, aov_error, aov_order, df, results)
