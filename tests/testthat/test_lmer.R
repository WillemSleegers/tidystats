
# Setup -------------------------------------------------------------------

# Load test data
test_results <- read_stats(system.file("test_data/lmer.json", 
  package = "tidystats"))

# Set options
tolerance <- 0.001

# Test: lmer --------------------------------------------------------------

test_that("lme4 works", {
  model <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lme4
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("lme4 ML works", {
  model <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy, 
    REML = FALSE)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lme4_ML
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("lme4 slopes works", {
  model <- lme4::lmer(Reaction ~ Days + (Days || Subject), lme4::sleepstudy)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lme4_slopes
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: anova.merMod ------------------------------------------------------

test_that("lme4 anova works", {
  lme4 <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  lme4_slopes <- lme4::lmer(Reaction ~ Days + (Days || Subject), 
    lme4::sleepstudy)
  model <- anova(lme4, lme4_slopes)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lmer_anova
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
