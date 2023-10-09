# Setup -------------------------------------------------------------------

expected_df <- readr::read_csv("../data/main_df.csv")

# Drop the spec_tbl_df class
expected_df <- expected_df[]

tolerance <- 0.001

# tidy_stats_to_data_frame() ----------------------------------------------

test_that("tidy stats to data frame works", {
  statistics <- read_stats("../data/main.json")

  df <- tidy_stats_to_data_frame(statistics)

  expect_equal(df, expected_df, tolerance = tolerance)
})
