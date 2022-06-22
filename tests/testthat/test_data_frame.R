
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Load test data
path <- system.file("tests/testthat/data/data_frame.json", package = "tidystats")
test_results <- read_stats(path)

# Set options
tolerance <- 0.001

# Test: data.frame() ----------------------------------------------------------

test_that("Data frame (mtcars) works", {
  model <- mtcars

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$data_frame_mtcars

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Data frame (ToothGrowth) works", {
  model <- ToothGrowth

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$data_frame_toothgrowth

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

data_frame_mix = data.frame(
  subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  strings = c('K', 'L', 'K', 'L', 'L', 'K', 'K', 'K', 'L', 'K'),
  facts = as.factor(c('K1', 'L1', 'K1', 'L2', 'L2', 'K2', 'K2', 'K1', 'L1', 'K2')),
  nums1 = c(2, 2, NA, NA, 1, 1, 1, 2, NA, NA),
  nums2 = c(6, 7, 8.5, NA, 5, 16, NA, 16, 45, 77)
)

test_that("Data frame (mixed variable types) works", {
  model <- data_frame_mix

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$data_frame_mix

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Data frame (with custom symbols) works", {
  model <- data_frame_mix

  tidy_model <- tidy_stats(model,
    symbols = list("facts" = "my_factors", "nums2" = "numbers 2"))
  tidy_model_test <- test_results$`data frame with custom symbol`

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Data frame (with custom subscripts) works", {
  model <- data_frame_mix

  tidy_model <- tidy_stats(model, 
    subscripts = c("rings", "2"),
    method_name = "Table from df with subscripts",
    table_name = "Headers with subscripts")
  tidy_model_test <- test_results$`data frame with subscripts`

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
