
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

# Add stats
results <- results %>%
  add_stats(aov) %>%
  add_stats(aov_order)

summary(aov)
summary(aov_order)

# aov(): aovlist ----------------------------------------------------------

# Run test
aov_error <- aov(yield ~ N * P * K + Error(block), npk)

# Add stats
results <- results %>%
  add_stats(aov_error)

summary(aov_error)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/aov.json")
