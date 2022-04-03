
# Setup -------------------------------------------------------------------

# Load test data
test_results <- read_stats(system.file("test_data/anova_lm.json", 
  package = "tidystats"))

# Set options
tolerance <- 0.001

# Test: anova.lm ----------------------------------------------------------

test_that("anova.lm works", {
  fit <- lm(sr ~ ., data = LifeCycleSavings)
  model <- anova(fit)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$anova_lm
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Run models
fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
fit1 <- update(fit0, . ~ . + pop15)
fit2 <- update(fit1, . ~ . + pop75)
fit3 <- update(fit2, . ~ . + dpi)
fit4 <- update(fit3, . ~ . + ddpi)

test_that("multiple models anova.lm works", {
  model <- anova(fit0, fit1, fit2, fit3, fit4, test = "F")
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$anova_lm_fits
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("unusual order anova.lm works", {
  model <- anova(fit4, fit2, fit0, test = "F")
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$anova_lm_order
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("chisq anova.lm works", {
  model <- anova(fit4, fit2, fit0, test = "Chisq")
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$anova_lm_chisq
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("cp anova.lm works", {
  model <- anova(fit4, fit2, fit0, test = "Cp")
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$anova_lm_cp
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
