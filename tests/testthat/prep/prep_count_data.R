
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
results <- list()

# count_data() ------------------------------------------------------------

# Run analyses
no_group <- count_data(quote_source)
single_group <- count_data(quote_source, source)
two_groups <- count_data(quote_source, source, sex)

grouped_group <- quote_source %>%
  group_by(source) %>%
  count_data(sex)

grouped_group_na_rm <- quote_source %>%
  group_by(source) %>%
  count_data(sex, na.rm = TRUE)

# Add stats
results <- results %>%
  add_stats(no_group) %>%
  add_stats(single_group) %>%
  add_stats(two_groups) %>%
  add_stats(grouped_group) %>%
  add_stats(grouped_group_na_rm) 

# Inspect output
no_group
single_group
two_groups
grouped_group
grouped_group_na_rm

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/count_data.json")

# Cleanup -----------------------------------------------------------------

rm(no_group, single_group, two_groups, grouped_group, grouped_group_na_rm, df,
  results)
