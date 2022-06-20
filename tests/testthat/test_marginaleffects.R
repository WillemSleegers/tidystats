
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(marginaleffects)

# Load test data
path <- system.file("tests/testthat/data/marginaleffects.json", package = "tidystats")
test_results <- read_stats(path)

# Set options
tolerance <- 0.001


# Test: marginaleffects() ----------------------------------------------------------

test_that("Average marginal effects works", {
  mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
  model <- summary(marginaleffects(mod))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$marginaleffects_summary

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: marginalmeans() ----------------------------------------------------------

test_that("Estimated marginal means works", {
  dat <- mtcars
  dat$cyl <- as.factor(dat$cyl)
  dat$am <- as.logical(dat$am)
  mod <- lm(mpg ~ hp + cyl + am, data = dat)
  model <- summary(marginalmeans(mod))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$marginalmeans_summary

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: comparisons() ----------------------------------------------------------

test_that("Average contrasts of marginal comparisons (LM) works", {
  tmp <- mtcars
  tmp$am <- as.logical(tmp$am)
  mod <- lm(mpg ~ am + factor(cyl), tmp)
  model <- summary(comparisons(mod, 
    contrast_factor = "reference", type = "response"))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$marginaleffects_comparisons_summary

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


test_that("Average contrasts of marginal comparisons (GLM) works", {
  mod <- glm(vs ~ mpg, data = mtcars, family = binomial)
  cmp <- comparisons(mod, transform_pre = "lnratioavg")
  model <- summary(cmp, transform_post = exp)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$marginaleffects_comparisons_summary_arr

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
