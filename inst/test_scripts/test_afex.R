
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(afex)

# Create an empty list
results <- list()

# Analysis: aov_ez -------------------------------------------------------

# Load data
data("fhch2010", package = "afex")
fhch <- fhch2010[ fhch2010$correct,]

# Run analysis
aov_ez <- aov_ez("id", "log_rt", fhch, between = "task", 
  within = c("stimulus", "length"))

# Tidy stats
temp <- tidy_stats(aov_ez)

# Add stats
results <- results %>%
  add_stats(aov_ez)

# Write stats
write_stats(results, "inst/test_data/afex.json")


