# Setup -------------------------------------------------------------------

statistics <- list()

# count_data() ------------------------------------------------------------

no_group <- count_data(quote_source)
single_group <- count_data(quote_source, source)
two_groups <- count_data(quote_source, source, sex, prop = TRUE)

grouped_group <- quote_source |>
  dplyr::group_by(source) |>
  count_data(sex)

grouped_group_na_rm <- quote_source |>
  dplyr::group_by(source) |>
  count_data(sex, na.rm = TRUE)

statistics <- statistics |>
  add_stats(no_group) |>
  add_stats(single_group) |>
  add_stats(two_groups) |>
  add_stats(grouped_group) |>
  add_stats(grouped_group_na_rm)

no_group
single_group
two_groups
grouped_group
grouped_group_na_rm

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/count_data.json")

# Cleanup -----------------------------------------------------------------

rm(
  no_group, single_group, two_groups, grouped_group, grouped_group_na_rm, df,
  statistics
)
