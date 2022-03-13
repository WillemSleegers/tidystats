
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# lm ----------------------------------------------------------------------

results <- list()

# Get data
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)

# Run analyses
lm <- lm(weight ~ group)
lm_wo_intercept <- lm(weight ~ group - 1) # omitting intercept

summary(lm)
summary(lm_wo_intercept)

# Tidy stats
temp <- tidy_stats(lm)
temp <- tidy_stats(lm_wo_intercept)

# Add stats
results <- results %>%
  add_stats(lm) %>%
  add_stats(lm_wo_intercept)

# write_stats() -----------------------------------------------------------

write_stats(results, "inst/test_data/lm.json")
