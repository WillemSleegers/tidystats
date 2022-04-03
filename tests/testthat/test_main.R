
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)

# Load test data
path <- system.file("tests/testthat/data/main.json", package = "tidystats")
test_results <- read_stats(path)

# Set options
tolerance <- 0.001

# Test: t-tests -----------------------------------------------------------

test_that("the t-test in main works", {
  model <- t.test(extra ~ group, data = sleep, paired = TRUE)
  
  results <- add_stats(list(), model, type = "primary")
  
  tidy_model <- results$model
  tidy_model_test <- test_results$sleep_test
  
  tidy_model$package <- NULL
  tidy_model_test$package <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("the linear regression in main works", {
  ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
  trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
  weight <- c(ctl, trt)
  
  model <- lm(weight ~ group)
  
  results <- add_stats(list(), model, preregistered = FALSE)
  
  tidy_model <- results$model
  tidy_model_test <- test_results$lm_D9
  
  tidy_model$package <- NULL
  tidy_model_test$package <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("the ANOVA in main works", {
  model <- aov(yield ~ block + N*P*K, npk)
  
  results <- add_stats(list(), model, notes = "An ANOVA example")
  
  tidy_model <- results$model
  tidy_model_test <- test_results$npk_aov
  
  tidy_model$package <- NULL
  tidy_model_test$package <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
