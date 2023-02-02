
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(effsize)

# Create an empty list
statistics <- list()

# cohen.d() ---------------------------------------------------------------

# Get data
set.seed(1)
treatment <- rnorm(100, mean = 10)
control <- rnorm(100, mean = 12)
d <- c(treatment, control)
f <- rep(c("Treatment", "Control"), each = 100)

# Run analyses
cohen_d <- cohen.d(d ~ f)
hedges_g <- cohen.d(d ~ f, hedges.correction = TRUE)

# Add stats
statistics <- statistics %>%
  add_stats(cohen_d) %>%
  add_stats(hedges_g)

# Inspect output
cohen_d
hedges_g

# VD.A() ------------------------------------------------------------------

# Run analyses
vda <- VD.A(d ~f)

# Add stats
statistics <- add_stats(statistics, vda)

# Inspect output
vda

# cliff.delta() -----------------------------------------------------------

# Get data
treatment <- c(10, 10, 20, 20, 20, 30, 30, 30, 40, 50)
control <- c(10, 20, 30, 40, 40, 50)

# Run analyses
cliffs_delta <- cliff.delta(treatment, control, return.dm = TRUE)

# Add stats
statistics <- add_stats(statistics, cliffs_delta)

# Inspect output
cliffs_delta

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/effsize.json")

# Cleanup -----------------------------------------------------------------

rm(
  statistics, control, d, f, treatment, cohen_d, hedges_g, vda, cliffs_delta, 
  df
)
