# Setup -------------------------------------------------------------------

path <- system.file("tests/data/main_df.csv", package = "tidystats")
expected_df <- readr::read_csv(path)

# Drop the spec_tbl_df class
expected_df <- expected_df[]

tolerance <- 0.001

# tidy_stats_to_data_frame() ----------------------------------------------

test_that("tidy stats to data frame works", {
  path <- system.file("tests/data/main.json", package = "tidystats")
  statistics <- read_stats(path)

  df <- tidy_stats_to_data_frame(statistics)

  expect_equal(df, expected_df, tolerance = tolerance)
})
