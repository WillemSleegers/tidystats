# Setup -------------------------------------------------------------------

statistics <- list()

# aov(): aov --------------------------------------------------------------

aov <- aov(yield ~ block + N * P * K, npk)
aov_order <- aov(terms(yield ~ block + N * P + K, keep.order = TRUE), npk)

statistics <- statistics |>
  add_stats(aov) |>
  add_stats(aov_order)

summary(aov)
summary(aov_order)

# aov(): aovlist ----------------------------------------------------------

aov_error <- aov(yield ~ N * P * K + Error(block), npk)

statistics <- add_stats(statistics, aov_error)

summary(aov_error)

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/aov.json")

# Cleanup -----------------------------------------------------------------

rm(aov, aov_error, aov_order, df, statistics)
