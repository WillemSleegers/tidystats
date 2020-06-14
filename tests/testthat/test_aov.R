
# Setup -------------------------------------------------------------------

# Load test data
test_results <- read_stats("aov_results.json")

# Set options
tolerance <- 0.001

# Test: One-way ANOVA -----------------------------------------------------

test_that("one-way ANOVAs works", {
  model <- aov(call_parent ~ condition, data = cox)
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$aov_one_way
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: two-way ANOVAs with interaction -----------------------------------

test_that("two-way ANOVAs with interaction works", {
  model <- aov(call_parent ~ condition * sex, data = cox)
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$aov_interaction
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: ANCOVA ------------------------------------------------------------

test_that("ANCOVAs works", {
  model <- aov(call_parent ~ condition + affect_negative, data = cox)
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$aov_ancova
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: One-way within-subjects ANOVA -------------------------------------

test_that("one-way within-subjects ANOVA works", {
  cox_long <- cox %>%
    gather("affect", "score", affect_positive, affect_negative) %>%
    mutate(
      ID = factor(ID),
      affect = factor(affect)
    )
  model <- aov(score ~ affect + Error(ID/affect), data = cox_long)
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$aov_within
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: Mixed ANOVA -------------------------------------------------------

test_that("one-way within-subjects ANOVA works", {
  cox_long <- cox %>%
    gather("affect", "score", affect_positive, affect_negative) %>%
    mutate(
      ID = factor(ID),
      affect = factor(affect)
    )
  model <- aov(score ~ condition * affect + Error(ID/affect) + condition,
    data = cox_long)
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$aov_mixed
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

