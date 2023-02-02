# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)

# Create an empty list
statistics <- list()

# stats() and stat() ------------------------------------------------------

# Run analyses
lm1 <- lm(Fertility ~ ., data = swiss)
lm2 <- update(lm1, . ~ . - Examination)

BF10 <- 1 / exp((BIC(lm2) - BIC(lm1)) / 2)

# Create the stats
stats <- stats(
  method = "BF BIC method",
  statistics = c(
    stat(name = "BF", value = BF10, subscript = "10"),
    stat(name = "BF", value = 1 / BF10, subscript = "01")
  )
)

# Add stats
statistics <- add_stats(
  list = statistics,
  output = stats,
  notes = "Wagenmakers (2007) method for calculating Bayes factors"
)

# Inspect output
stats

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/generic.json")

# Cleanup -----------------------------------------------------------------

rm(BIC_AIC, df, statistics)
