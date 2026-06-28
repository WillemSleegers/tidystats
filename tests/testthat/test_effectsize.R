if (requireNamespace("effectsize", quietly = TRUE)) library(effectsize)

# Compare against the effectsize object's own value (its first column) rather
# than hard-coded constants, so the tests stay correct if effectsize changes
# how it computes the effect size across versions.

# cohens_d() --------------------------------------------------------------

test_that("effectsize's Cohen's d works", {
  skip_if_not_installed("effectsize")
  es <- cohens_d(mpg ~ am, data = mtcars)
  result <- tidy_stats(es)

  expect_equal(result$method, "Cohen's d effect size")
  expect_equal(result$statistics[[1]]$value, es[[1]])
})

test_that("effectsize's Cohen's d not pooled works", {
  skip_if_not_installed("effectsize")
  es <- cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

test_that("effectsize's Cohen's d mu works", {
  skip_if_not_installed("effectsize")
  es <- cohens_d(mpg ~ am, data = mtcars, mu = -5)
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

test_that("effectsize's Cohen's d less works", {
  skip_if_not_installed("effectsize")
  es <- cohens_d(mpg ~ am, data = mtcars, alternative = "less")
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

test_that("effectsize's Cohen's d one sample works", {
  skip_if_not_installed("effectsize")
  es <- cohens_d(wt ~ 1, data = mtcars)
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

test_that("effectsize's Cohen's d paired works", {
  skip_if_not_installed("effectsize")
  es <- cohens_d(
    Pair(extra[group == 1], extra[group == 2]) ~ 1,
    data = sleep
  )
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

# hedges_g() --------------------------------------------------------------

test_that("effectsize's Hedges' g works", {
  skip_if_not_installed("effectsize")
  es <- hedges_g(mpg ~ am, data = mtcars)
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

test_that("effectsize's Hedges' g not pooled works", {
  skip_if_not_installed("effectsize")
  es <- hedges_g(mpg ~ am, data = mtcars, pooled_sd = FALSE)
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

test_that("effectsize's Hedges' g mu works", {
  skip_if_not_installed("effectsize")
  es <- hedges_g(mpg ~ am, data = mtcars, mu = -5)
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

test_that("effectsize's Hedges' g less works", {
  skip_if_not_installed("effectsize")
  es <- hedges_g(mpg ~ am, data = mtcars, alternative = "less")
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

test_that("effectsize's Hedges' g one sample works", {
  skip_if_not_installed("effectsize")
  es <- hedges_g(wt ~ 1, data = mtcars)
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

test_that("effectsize's Hedges' g paired works", {
  skip_if_not_installed("effectsize")
  es <- hedges_g(
    Pair(extra[group == 1], extra[group == 2]) ~ 1,
    data = sleep
  )
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

# glass_delta() --------------------------------------------------------------

test_that("effectsize's Glass's delta works", {
  skip_if_not_installed("effectsize")
  es <- glass_delta(mpg ~ am, data = mtcars)
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

test_that("effectsize's Glass's delta mu works", {
  skip_if_not_installed("effectsize")
  es <- glass_delta(mpg ~ am, data = mtcars, mu = -5)
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})

test_that("effectsize's Glass's delta less works", {
  skip_if_not_installed("effectsize")
  es <- glass_delta(mpg ~ am, data = mtcars, alternative = "less")
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es[[1]])
})
