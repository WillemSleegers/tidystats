
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create the list
results <- list()

# Run analyses ------------------------------------------------------------

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

single_var
single_var_subset
single_var_w_group
single_var_w_groups
single_var_w_groups_wo_na
multiple_var
multiple_var_w_group

# Add stats ---------------------------------------------------------------

results <- results %>%
  add_stats(single_var) %>%
  add_stats(single_var_w_group) %>%
  add_stats(single_var_w_groups) %>%
  add_stats(single_var_subset)

# Write stats -------------------------------------------------------------

write_stats(results, "tests/testthat/data/results.json")

# Convert to data frame ---------------------------------------------------

df <- tidystats_to_data_frame(results)
write_csv(df, "tests/testthat/data/results_df.csv")

# Cleanup -----------------------------------------------------------------

rm(sleep_test, lm_D9, npk_aov, results, df)

