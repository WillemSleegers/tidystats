
# Setup -------------------------------------------------------------------

# Load packages
library(readr)

# Load test data
path <- system.file("tests/testthat/data/main_df.csv", package = "tidystats")
expected_df <- read_csv(path)

# Drop the spec_tbl_df class
expected_df <- expected_df[]

# Set options
tolerance <- 0.001

# tidy_stats_to_data_frame() ----------------------------------------------

test_that("tidy stats to data frame works", {
  path <- system.file("tests/testthat/data/main.json", package = "tidystats")
  statistics <- read_stats(path)
  
  df <- tidy_stats_to_data_frame(statistics)
  
  expect_equal(df, expected_df, tolerance = tolerance)
})
