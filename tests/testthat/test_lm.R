
# Setup -------------------------------------------------------------------

# Load test data
test_results <- read_stats("lm_results.json")

# Set options
tolerance <- 0.001

# Test: Simple regression -------------------------------------------------

test_that("simple regression works", {
  model <- lm(call_parent ~ condition, data = cox)
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lm_simple
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: Multiple regression -----------------------------------------------

test_that("multiple regression works", {
  model <- lm(call_parent ~ condition + anxiety, data = cox)
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lm_multiple
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("multiple regression with interaction works", {
  model <- lm(call_parent ~ condition * anxiety, data = cox)
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lm_interaction
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
