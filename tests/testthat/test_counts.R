
# Setup -------------------------------------------------------------------

# Load packages
library(dplyr)

# Load test data
test_results <- read_stats(system.file("test_data/count_data.json", 
  package = "tidystats"))

# Set options
tolerance <- 0.001

# Test: describe_data() ---------------------------------------------------

test_that("single group counts works", {
  single_group <- count_data(quote_source, source)
  
  tidy_descriptives <- tidy_stats(single_group)
  tidy_descriptives_test <- test_results$single_group
  
  tidy_descriptives$package$version <- NULL
  tidy_descriptives_test$package$version <- NULL
  
  expect_equal(tidy_descriptives, tidy_descriptives_test, tolerance = tolerance)
})

test_that("two group counts works", {
  two_groups <- count_data(quote_source, source, sex)
  
  tidy_descriptives <- tidy_stats(two_groups)
  tidy_descriptives_test <- test_results$two_groups
  
  tidy_descriptives$package$version <- NULL
  tidy_descriptives_test$package$version <- NULL
  
  expect_equal(tidy_descriptives, tidy_descriptives_test, tolerance = tolerance)
})

test_that("grouped group counts works", {
  grouped_group <- quote_source %>%
    group_by(source) %>%
    count_data(sex)
  
  tidy_descriptives <- tidy_stats(grouped_group)
  tidy_descriptives_test <- test_results$grouped_group
  
  tidy_descriptives$package$version <- NULL
  tidy_descriptives_test$package$version <- NULL
  
  expect_equal(tidy_descriptives, tidy_descriptives_test, tolerance = tolerance)
})

test_that("grouped group counts without missings works", {
  grouped_group_na_rm <- quote_source %>%
    group_by(source) %>%
    count_data(sex, na.rm = TRUE)
  
  tidy_descriptives <- tidy_stats(grouped_group_na_rm)
  tidy_descriptives_test <- test_results$grouped_group_na_rm
  
  tidy_descriptives$package$version <- NULL
  tidy_descriptives_test$package$version <- NULL
  
  expect_equal(tidy_descriptives, tidy_descriptives_test, tolerance = tolerance)
})