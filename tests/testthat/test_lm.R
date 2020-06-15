
# Setup -------------------------------------------------------------------

# Load test data
test_results <- read_stats(system.file("test_data/lm_results.json", 
  package = "tidystats"))

# Set options
tolerance <- 0.001

# Test: Simple regression -------------------------------------------------

test_that("simple regression works", {
  model <- lm(call_parent ~ condition, data = cox)
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lm_simple
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: Multiple regression -----------------------------------------------

test_that("multiple regression works", {
  model <- lm(call_parent ~ condition + anxiety, data = cox)
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lm_multiple
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("multiple regression with interaction works", {
  model <- lm(call_parent ~ condition * anxiety, data = cox)
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lm_interaction
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
