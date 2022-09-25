
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

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_test_stats(results, "tests/testthat/data/generic.json")

# Cleanup -----------------------------------------------------------------

rm(generic_ICs, df, results)
