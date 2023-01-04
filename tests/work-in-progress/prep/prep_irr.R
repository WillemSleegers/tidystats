
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(irr)

# Load data
data(anxiety)

set.seed(1)
r1 <- round(rnorm(20, 10, 4))
r2 <- round(r1 + 10 + rnorm(20, 0, 2))
r3 <- round(r1 + 20 + rnorm(20, 0, 2))

# Create empty list
results <- list()

# icc ---------------------------------------------------------------------

# Run test
ICC_anxiety <- icc(anxiety, model = "twoway", type = "agreement")

ICC_high_consistency <- icc(cbind(r1, r2, r3), "twoway")
ICC_low_agreement <- icc(cbind(r1, r2, r3), "twoway", "agreement")

# Tidy results
temp <- tidy_stats(ICC_anxiety)
temp <- tidy_stats(ICC_high_consistency)
temp <- tidy_stats(ICC_low_agreement)

# Add stats
results <- results %>%
  add_stats(ICC_anxiety) %>%
  add_stats(ICC_high_consistency) %>%
  add_stats(ICC_low_agreement)

# Write stats
write_stats(results, "inst/test_data/irr.json")
