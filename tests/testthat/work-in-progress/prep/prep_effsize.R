
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(effsize)

# Create an empty list
results <- list()

# cohen.d -----------------------------------------------------------------

# Get data
set.seed(1)
treatment = rnorm(100,mean=10)
control = rnorm(100,mean=12)
d = (c(treatment,control))
f = rep(c("Treatment","Control"),each=100)

# Run model
cohen_d <- cohen.d(d ~ f)
cohen_d_hedges <- cohen.d(d ~ f, hedges.correction = TRUE)

cohen_d
cohen_d_hedges

# Tidy stats
temp <- tidy_stats(cohen_d)
temp <- tidy_stats(cohen_d_hedges)

# Add stats
results <- results %>%
  add_stats(cohen_d) %>%
  add_stats(cohen_d_hedges)

# Save stats
write_stats(results, "inst/test_data/effsize.json")
