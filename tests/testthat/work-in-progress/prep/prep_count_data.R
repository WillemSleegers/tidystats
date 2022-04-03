# Analysis: count_data() --------------------------------------------------

results <- list()

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

no_group
single_group
two_groups
grouped_group
grouped_group_na_rm

# Tidy stats
temp <- tidy_stats(no_group)
temp <- tidy_stats(single_group)
temp <- tidy_stats(two_groups)
temp <- tidy_stats(grouped_group)
temp <- tidy_stats(grouped_group_na_rm)

# Add stats
results <- results %>%
  add_stats(no_group) %>%
  add_stats(single_group) %>%
  add_stats(two_groups) %>%
  add_stats(grouped_group) %>%
  add_stats(grouped_group_na_rm) 

write_stats(results, "inst/test_data/count_data.json")