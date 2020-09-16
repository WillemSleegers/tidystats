
# Setup -------------------------------------------------------------------

# Load test data
test_results <- read_stats(system.file("test_data/lm.json", 
  package = "tidystats"))

# Load data
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)

# Set options
tolerance <- 0.001

# Test: lm ----------------------------------------------------------------

test_that("lm works", {
  model <- lm(weight ~ group)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lm
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("lm without an intercept works", {
  model <- lm(weight ~ group - 1)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lm_wo_intercept
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
