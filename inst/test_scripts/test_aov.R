
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
results <- list()

# aov: aov ----------------------------------------------------------------

# Run tests
aov <- aov(yield ~ block + N * P * K, npk)
aov_order <- aov(terms(yield ~ block + N * P + K, keep.order = TRUE), npk)

summary(aov)
summary(aov_order)

# Tidy stats
temp <- tidy_stats(aov)
temp <- tidy_stats(aov_order)

# Add stats
results <- results %>%
  add_stats(aov) %>%
  add_stats(aov_order)

# aov: aovlist ------------------------------------------------------------

# Run test
aov_error <- aov(yield ~ N * P * K + Error(block), npk)

summary(aov_error)

# Tidy stats
temp <- tidy_stats(aov_error)

# Add stats
results <- results %>%
  add_stats(aov_error)

# aov: write_stats() ------------------------------------------------------

write_stats(results, "inst/test_data/aov.json")
