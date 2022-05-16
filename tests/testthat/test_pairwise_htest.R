
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Load test data
path <- system.file("tests/testthat/data/pairwise_htest.json", package = "tidystats")
test_results <- read_stats(path)

# Set options
tolerance <- 0.001


# Test: pairwise.t.test() ----------------------------------------------------------

Month <- factor(airquality$Month, labels = month.abb[5:9])

test_that("Pairwise t tests with pooled SD works", {
  model <- pairwise.t.test(airquality$Ozone, Month)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$pairwise_t_test

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Pairwise paired t tests works", {
  model <- pairwise.t.test(c(1,2,3,1,2,4), c(1,1,2,2,3,3), paired = TRUE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$pairwise_t_test_paired

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Pairwise t tests with non-pooled SD works", {
  model <- pairwise.t.test(airquality$Ozone,
    Month,
    p.adjust.method = "bonf",
    pool.sd = FALSE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$pairwise_t_test_nonpooled

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: pairwise.prop.test() ----------------------------------------------------------

smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )

test_that("Pairwise comparison of proportions works", {
  model <- suppressWarnings(pairwise.prop.test(smokers, patients))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$pairwise_prop_test

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: pairwise.wilcox.test() ----------------------------------------------------------

test_that("Pairwise Wilcoxon rank sum exact test works", {
  model <- pairwise.wilcox.test(c(1,2,3,4,5,6,7,8,9,11), c(1,1,1,1,1,2,2,2,2,2))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$pairwise_wilcox_test

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Pairwise Wilcoxon signed rank exact test works", {
  model <- pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
    p.adjust.method = "BH", paired = TRUE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$pairwise_wilcox_test_paired

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
