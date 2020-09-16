
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# add_stats() -------------------------------------------------------------

# Conduct three different analyses
# t-test:
sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)

# lm:
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm_D9 <- lm(weight ~ group)

# ANOVA:
npk_aov <- aov(yield ~ block + N*P*K, npk)

# Create an empty list
results <- list()

# Add the analyses to the empty list
results <- results %>%
  add_stats(sleep_test, type = "primary") %>%
  add_stats(lm_D9, preregistered = FALSE) %>%
  add_stats(npk_aov, notes = "An ANOVA example")

# write_stats() -----------------------------------------------------------

write_stats(results, "inst/results.json")

# tidy_stats_to_data_frame ------------------------------------------------

df <- tidy_stats_to_data_frame(results)
write_csv(df, "inst/results_df.csv")

# Analysis: describe_data() -------------------------------------------

results <- list()

# Single variable descriptives 
single_var <- describe_data(quote_source, response)
single_var

# Single variable with group descriptives
single_var_w_group <- quote_source %>%
  group_by(source) %>%
  describe_data(response)
single_var_w_group

# Single variable with multiple group descriptives
single_var_w_groups <- quote_source %>%
  group_by(source, sex) %>%
  describe_data(response)
single_var_w_groups

# Subset of descriptives
single_var_subset <- describe_data(quote_source, response, short = TRUE)
single_var_subset

# Tidy stats
temp <- tidy_stats(single_var)
temp <- tidy_stats(single_var_w_group)
temp <- tidy_stats(single_var_w_groups)
temp <- tidy_stats(single_var_subset)

# Add stats
results <- results %>%
  add_stats(single_var) %>%
  add_stats(single_var_w_group) %>%
  add_stats(single_var_w_groups) %>%
  add_stats(single_var_subset)

write_stats(results, "inst/test_data/describe_data.json")

# Analysis: count_data ----------------------------------------------------

results <- list()

# Single group
single_group <- count_data(quote_source, source)
single_group

# Two groups
two_groups <- count_data(quote_source, source, sex)
two_groups

# Grouped groups
grouped_group <- quote_source %>%
  group_by(source) %>%
  count_data(sex)
grouped_group

# Omit missings
grouped_group_na_rm <- quote_source %>%
  group_by(source) %>%
  count_data(sex, na.rm = TRUE)
grouped_group_na_rm

# Tidy stats
temp <- tidy_stats(single_group)
temp <- tidy_stats(two_groups)
temp <- tidy_stats(grouped_group)
temp <- tidy_stats(grouped_group_na_rm)

# Add stats
results <- results %>%
  add_stats(single_group) %>%
  add_stats(two_groups) %>%
  add_stats(grouped_group) %>%
  add_stats(grouped_group_na_rm) 

write_stats(results, "inst/test_data/count_data.json")

# Analysis: generic test --------------------------------------------------

results <- list()

# Create generic tests
generic_ICs <- list(
  statistics = list(
    BIC = 21.21,
    AIC = 478.21
  )
)

generic_CIs <- list(
  statistics = list(
    CI = list(
      CI_level = .95,
      CI_upper = .64,
      CI_lower = .32
    )
  )
)

# Add it to the list
results <- results %>%
    add_stats(generic_ICs, 
      notes = "Wagenmakers (2007) method for calculating Bayes factors") %>%
    add_stats(generic_CIs, notes = "Just some random CIs")

write_stats(results, "inst/test_data/generic.json")
