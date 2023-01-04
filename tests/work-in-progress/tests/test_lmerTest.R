
# Setup -------------------------------------------------------------------

# Load test data
test_results <- read_stats(system.file("test_data/lmerTest.json", 
  package = "tidystats"))

# Set options
tolerance <- 0.001

# Test: lmerTest ----------------------------------------------------------

test_that("lmerTest works", {
  model <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lmerTest1
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("another lmerTest works", {
  model <- lmerTest::lmer(Informed.liking ~ Gender + Information * 
    Product + (1 | Consumer) + (1 | Consumer:Product), data = lmerTest::ham)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$lmerTest2
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: anova.lmerModLmerTest ---------------------------------------------

test_that("lmerTest anova works", {
  m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  model <- anova(m)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$anova_lmerTest
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("lmerTest lme4 anova works", {
  m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  model <- anova(m, ddf = "lme4")
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$anova_lmerTest_lme4
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("lmerTest anova fit works", {
  m0 <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  model <- anova(m0, m)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$anova_lmerTest_fit
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

