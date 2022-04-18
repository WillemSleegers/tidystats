
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
results <- list()

# describe_data() ---------------------------------------------------------

# Run analyses
single_var <- describe_data(quote_source, response)

single_var_w_group <- quote_source %>%
  group_by(source) %>%
  describe_data(response)

multiple_var <- describe_data(quote_source, response, age)

single_var_w_groups <- quote_source %>%
  group_by(source, sex) %>%
  describe_data(response)

single_var_w_groups_wo_na <- quote_source %>%
  group_by(source, sex) %>%
  describe_data(response, na.rm = FALSE)

multiple_var_w_group <- quote_source %>%
  group_by(source) %>%
  describe_data(response, age)

single_var_subset <- describe_data(quote_source, response, short = TRUE)

# add_stats() -------------------------------------------------------------

results <- results %>%
  add_stats(single_var) %>%
  add_stats(single_var_w_group) %>%
  add_stats(multiple_var) %>%
  add_stats(single_var_w_groups) %>%
  add_stats(single_var_w_groups_wo_na) %>%
  add_stats(multiple_var_w_group) %>%
  add_stats(single_var_subset)

# Inspect output ----------------------------------------------------------

single_var
single_var_subset
single_var_w_group
single_var_w_groups
single_var_w_groups_wo_na
multiple_var
multiple_var_w_group

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/results.json")

# Cleanup -----------------------------------------------------------------

rm(sleep_test, lm_D9, npk_aov, results, df)
