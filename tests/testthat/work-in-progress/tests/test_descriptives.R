
# Setup -------------------------------------------------------------------

# Load packages
library(dplyr)

# Load test data
test_results <- read_stats(system.file("test_data/describe_data.json", 
  package = "tidystats"))

# Set options
tolerance <- 0.001

# Test: describe_data() ---------------------------------------------------

test_that("single var descriptives works", {
  single_var <- describe_data(quote_source, response)
  
  tidy_descriptives <- tidy_stats(single_var)
  tidy_descriptives_test <- test_results$single_var
  
  tidy_descriptives$package$version <- NULL
  tidy_descriptives_test$package$version <- NULL
  
  expect_equal(tidy_descriptives, tidy_descriptives_test, tolerance = tolerance)
})

test_that("single var with group descriptives works", {
  single_var_w_group <- quote_source %>%
    group_by(source) %>%
    describe_data(response)
  
  tidy_descriptives <- tidy_stats(single_var_w_group)
  tidy_descriptives_test <- test_results$single_var_w_group
  
  tidy_descriptives$package$version <- NULL
  tidy_descriptives_test$package$version <- NULL
  
  expect_equal(tidy_descriptives, tidy_descriptives_test, tolerance = tolerance)
})

test_that("single var with multiple group descriptives works", {
  single_var_w_groups <- quote_source %>%
    group_by(source, sex) %>%
    describe_data(response)
  
  tidy_descriptives <- tidy_stats(single_var_w_groups)
  tidy_descriptives_test <- test_results$single_var_w_groups
  
  tidy_descriptives$package$version <- NULL
  tidy_descriptives_test$package$version <- NULL
  
  expect_equal(tidy_descriptives, tidy_descriptives_test, tolerance = tolerance)
})

test_that("single var subset descriptives works", {
  single_var_subset <- describe_data(quote_source, response, short = TRUE)
  
  tidy_descriptives <- tidy_stats(single_var_subset)
  tidy_descriptives_test <- test_results$single_var_subset
  
  tidy_descriptives$package$version <- NULL
  tidy_descriptives_test$package$version <- NULL
  
  expect_equal(tidy_descriptives, tidy_descriptives_test, tolerance = tolerance)
})