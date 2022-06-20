
# Setup -------------------------------------------------------------------

# Load test data
path <- system.file("tests/testthat/data/aov.json", package = "tidystats")
test_results <- read_stats(path)

# Test: aov ---------------------------------------------------------------

test_that("aov works", {
  model <- aov(yield ~ block + N * P * K, npk)
  
  expect_equal_models(
    model = model, 
    tidy_model_test = test_results$aov
  )
})

test_that("aov order works", {
  model <- aov(terms(yield ~ block + N * P + K, keep.order = TRUE), npk)
  
  expect_equal_models(
    model = model, 
    tidy_model_test = test_results$aov_order
  )
})

test_that("aov error works", {
  model <- aov(yield ~ N * P * K + Error(block), npk)
  
  expect_equal_models(
    model = model, 
    tidy_model_test = test_results$aov_error
  )
})
