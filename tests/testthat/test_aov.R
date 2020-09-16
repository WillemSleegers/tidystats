
# Setup -------------------------------------------------------------------

# Load test data
test_results <- read_stats(system.file("test_data/aov.json", 
  package = "tidystats"))

# Set options
tolerance <- 0.001

# Test: aov ---------------------------------------------------------------

test_that("aov works", {
  model <- aov(yield ~ block + N * P * K, npk)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$aov
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("aov order works", {
  model <- aov(terms(yield ~ block + N * P + K, keep.order = TRUE), npk)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$aov_order
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("aov error works", {
  model <- aov(yield ~ N * P * K + Error(block), npk)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$aov_error
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
