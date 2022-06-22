
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(emmeans)

# Load test data
path <- system.file("tests/testthat/data/emmeans.json", package = "tidystats")
test_results <- read_stats(path)

# Set options
tolerance <- 0.001


# Test: emmeans() ----------------------------------------------------------

test_that("Estimated marginal means (single set) works", {
  model <- emmeans(lm(breaks ~ wool, data = warpbreaks),  ~ wool)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_single

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
test_that("Estimated marginal means (multiple sets) works", {
  model <- emmeans(warp.lm,  ~ wool | tension)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_multi

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

emmeans_adjust <- emmeans(warp.lm, poly ~ tension | wool, adjust = "sidak")

test_that("Estimated marginal means (from list) works", {
  model <- emmeans_adjust

  tidy_model <- add_stats(list(), model)$`model$emmeans`
  tidy_model_test <- test_results$`emmeans_adjust$emmeans`

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Estimated marginal means (contrasts) works", {
  model <- emmeans_adjust

  tidy_model <- add_stats(list(), model)$`model$contrasts`
  tidy_model_test <- test_results$`emmeans_adjust$contrasts`

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: summary(emmeans()) ----------------------------------------------------------

test_that("Estimated marginal means (single set) works", {
  model <- summary(emmeans(lm(breaks ~ wool, data = warpbreaks),  ~ wool))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_summary_single

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
test_that("Estimated marginal means (multiple sets) works", {
  model <- summary(emmeans(warp.lm,  ~ wool | tension))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_summary_multi

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

emmeans_summary_adjust <- summary(emmeans(warp.lm, poly ~ tension | wool, adjust = "sidak"))

test_that("Estimated marginal means (from list) works", {
  model <- emmeans_summary_adjust

  tidy_model <- add_stats(list(), model)$`model$emmeans`
  tidy_model_test <- test_results$`emmeans_summary_adjust$emmeans`

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Estimated marginal means (contrasts) works", {
  model <- emmeans_summary_adjust

  tidy_model <- add_stats(list(), model)$`model$contrasts`
  tidy_model_test <- test_results$`emmeans_summary_adjust$contrasts`

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: confint(emmeans()) ----------------------------------------------------------

warp.emm <- emmeans(warp.lm, ~ tension | wool)

test_that("Confidence interval for estimated marginal means works", {
  model <- confint(warp.emm, level = .85)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_confint

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


test_that("Confidence interval for estimated marginal means (single set) works", {
  model <- confint(warp.emm, by = NULL, level = .90)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_confint_single

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: test(emmeans()) ----------------------------------------------------------

pigs.lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
pigs.emm <- emmeans(pigs.lm, "percent", type = "response")

test_that("Test for estimated marginal means works", {
  model <- test(pigs.emm, null = log(35), delta = log(1.10), side = ">")

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_test

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
test_that("Test (joint) for estimated marginal means works", {
  model <- test(pigs.emm, joint = TRUE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_test_joint

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: contrast() ----------------------------------------------------------

test_that("Contrast for estimated marginal means works", {
  set.seed(1234)
  pigs.lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
  pigs.emm <- emmeans(pigs.lm, "percent", type = "response")
  model <- contrast(pigs.emm, "consec")

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_contrast

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: mvcontrast() ----------------------------------------------------------

MOats.lm <- lm(yield ~ Variety + Block, data = MOats)
MOats.emm <- emmeans(MOats.lm, ~ Variety | rep.meas)
emmeans_mvcontrast <- mvcontrast(MOats.emm, "consec", show.ests = TRUE) 

test_that("Multivariate contrasts (estimates) for estimated marginal means works", {
  model <- emmeans_mvcontrast

  tidy_model <- add_stats(list(), model)$`model$estimates`
  tidy_model_test <- test_results$`emmeans_mvcontrast$estimates`
  
  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Multivariate contrasts (estimates) for estimated marginal means works", {
  model <- emmeans_mvcontrast

  tidy_model <- add_stats(list(), model)$`model$tests`
  tidy_model_test <- test_results$`emmeans_mvcontrast$tests`
  
  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Multivariate contrasts for estimated marginal means (named) works", {
  model <- mvcontrast(MOats.emm, "identity", name = "Variety", null = c(80, 100, 120, 140))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_mvcontrast_named

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: eff_size() ----------------------------------------------------------

test_that("Effect size calculation for estimated marginal means works", {
  fiber.lm <- lm(strength ~ diameter + machine, data = fiber)
  emm <- emmeans(fiber.lm, "machine")
  model <- eff_size(emm, sigma = sigma(fiber.lm), edf = df.residual(fiber.lm))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_eff_size

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})



# Test: emtrends() ----------------------------------------------------------

test_that("Estimated marginal means of linear trends works", {
  fiber.lm <- lm(strength ~ diameter*machine, data=fiber)
  model <- emtrends(fiber.lm, ~ machine | diameter, var = "sqrt(diameter)", at = list(diameter = c(20, 30)))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emtrends_basic

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Estimated marginal means of linear trends (with covariates) works", {
  mtcars.lm <- lm(mpg ~ poly(disp, degree = 2) * (factor(cyl) + factor(am)), data = mtcars)
  model <- emtrends(mtcars.lm, var = "disp", cov.reduce = disp ~ factor(cyl))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emtrends_cov_reduce

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: joint_tests() ----------------------------------------------------------

pigs.lm <- lm(log(conc) ~ source * factor(percent), data = pigs)

test_that("Computing joint tests of the terms in a model works", {
  model <- joint_tests(pigs.lm)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$joint_tests_single

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


test_that("Computing joint tests of the terms in a model (with separated sets) works", {
  model <- joint_tests(pigs.lm, by = "source")

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$joint_tests_multi

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: ref_grid() ----------------------------------------------------------

test_that("Computing joint tests of the terms in a model works", {
  fiber.lm <- lm(strength ~ machine*diameter, data = fiber)
  model <- ref_grid(fiber.lm)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$ref_grid_results

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
