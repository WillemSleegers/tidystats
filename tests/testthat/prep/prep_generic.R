
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
statistics <- list()

# BIC/AIC -----------------------------------------------------------------

# Create generic list of statistics
BIC_AIC <- list(
  name = "BIC/AIC",
  statistics = list(
    list(name = "BIC", value = 21.21),
    list(name = "AIC", value = 478.21)
  )
)

# Add stats
statistics <- add_stats(
  list = statistics, 
  output = BIC_AIC, 
  notes = "Wagenmakers (2007) method for calculating Bayes factors"
)

# Inspect output
statistics

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/testthat/data/generic.json")

# Cleanup -----------------------------------------------------------------

rm(BIC_AIC, df, statistics)
