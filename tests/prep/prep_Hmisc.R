
# Setup -------------------------------------------------------------------

# Load packages
library(Hmisc)
library(tidyverse)

# Create an empty list
statistics <- list()

# rcorr() -----------------------------------------------------------------

x <- c(-2, -1, 0, 1, 2)
y <- c(4, 1, 0, 1, 4)
z <- c(1, 2, 3, 4, NA)
v <- c(1, 2, 3, 4, 5)

rcorr <- rcorr(cbind(x, y, z, v), type = "pearson")
rcorr_spearman <- rcorr(cbind(x, y, z, v), type = "spearman")

statistics <- statistics %>%
  add_stats(rcorr) %>%
  add_stats(rcorr_spearman)

rcorr
rcorr_spearman

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/Hmisc.json")

# Cleanup -----------------------------------------------------------------

rm(x, y, z, v, rcorr, rcorr_spearman, df, statistics)
