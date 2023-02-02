
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)

# Create an empty list
statistics <- list()

# describe_data() ---------------------------------------------------------

# Run analyses
single_var <- describe_data(quote_source, response)

single_var_w_group <- quote_source %>%
  group_by(source) %>%
  describe_data(response)

multiple_vars <- describe_data(quote_source, response, age)

single_var_w_groups <- quote_source %>%
  group_by(source, sex) %>%
  describe_data(response)

single_var_w_groups_wo_na <- quote_source %>%
  group_by(source, sex) %>%
  describe_data(response, na.rm = FALSE)

multiple_vars_w_group <- quote_source %>%
  group_by(source) %>%
  describe_data(response, age)

single_var_subset <- describe_data(quote_source, response, short = TRUE)

# Add stats
statistics <- statistics %>%
  add_stats(single_var) %>%
  add_stats(single_var_w_group) %>%
  add_stats(multiple_vars) %>%
  add_stats(single_var_w_groups) %>%
  add_stats(single_var_w_groups_wo_na) %>%
  add_stats(multiple_vars_w_group) %>%
  add_stats(single_var_subset)

# Inspect output
single_var
single_var_subset
single_var_w_group
single_var_w_groups
single_var_w_groups_wo_na
multiple_vars
multiple_vars_w_group

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/describe_data.json")

# Cleanup -----------------------------------------------------------------

rm(
  single_var, single_var_subset, single_var_w_group, single_var_w_groups, 
  single_var_w_groups_wo_na, multiple_vars, multiple_vars_w_group, df, 
  statistics
) 
