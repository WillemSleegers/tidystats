if (requireNamespace("effectsize", quietly = TRUE)) library(effectsize)

# cohens_d() --------------------------------------------------------------

test_that("effectsize's Cohen's d works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(cohens_d(mpg ~ am, data = mtcars))

  expect_equal(result$method, "Cohen's d effect size")
  expect_equal(result$statistics[[1]]$value, -1.477947, tolerance = 1e-4)
})

test_that("effectsize's Cohen's d not pooled works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE))

  expect_equal(result$statistics[[1]]$value, -1.411046, tolerance = 1e-4)
})

test_that("effectsize's Cohen's d mu works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(cohens_d(mpg ~ am, data = mtcars, mu = -5))

  expect_equal(result$statistics[[1]]$value, -0.4579613, tolerance = 1e-4)
})

test_that("effectsize's Cohen's d less works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(cohens_d(mpg ~ am, data = mtcars, alternative = "less"))

  expect_equal(result$statistics[[1]]$value, -1.477947, tolerance = 1e-4)
})

test_that("effectsize's Cohen's d one sample works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(cohens_d(wt ~ 1, data = mtcars))

  expect_equal(result$statistics[[1]]$value, 3.288084, tolerance = 1e-4)
})

test_that("effectsize's Cohen's d paired works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(cohens_d(
    Pair(extra[group == 1], extra[group == 2]) ~ 1,
    data = sleep
  ))

  expect_equal(result$statistics[[1]]$value, -1.284558, tolerance = 1e-4)
})

# hedges_g() --------------------------------------------------------------

test_that("effectsize's Hedges' g works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(hedges_g(mpg ~ am, data = mtcars))

  expect_equal(result$statistics[[1]]$value, -1.440635, tolerance = 1e-4)
})

test_that("effectsize's Hedges' g not pooled works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(hedges_g(mpg ~ am, data = mtcars, pooled_sd = FALSE))

  expect_equal(result$statistics[[1]]$value, -1.352384, tolerance = 1e-4)
})

test_that("effectsize's Hedges' g mu works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(hedges_g(mpg ~ am, data = mtcars, mu = -5))

  expect_equal(result$statistics[[1]]$value, -0.4463997, tolerance = 1e-4)
})

test_that("effectsize's Hedges' g less works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(hedges_g(mpg ~ am, data = mtcars, alternative = "less"))

  expect_equal(result$statistics[[1]]$value, -1.440635, tolerance = 1e-4)
})

test_that("effectsize's Hedges' g one sample works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(hedges_g(wt ~ 1, data = mtcars))

  expect_equal(result$statistics[[1]]$value, 3.207777, tolerance = 1e-4)
})

test_that("effectsize's Hedges' g paired works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(hedges_g(
    Pair(extra[group == 1], extra[group == 2]) ~ 1,
    data = sleep
  ))

  expect_equal(result$statistics[[1]]$value, -1.173925, tolerance = 1e-4)
})

# glass_delta() --------------------------------------------------------------

test_that("effectsize's Glass's delta works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(glass_delta(mpg ~ am, data = mtcars))

  expect_equal(result$statistics[[1]]$value, -1.099625, tolerance = 1e-4)
})

test_that("effectsize's Glass's delta mu works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(glass_delta(mpg ~ am, data = mtcars, mu = -5))

  expect_equal(result$statistics[[1]]$value, -0.3407332, tolerance = 1e-4)
})

test_that("effectsize's Glass's delta less works", {
  skip_if_not_installed("effectsize")
  result <- tidy_stats(glass_delta(
    mpg ~ am,
    data = mtcars, alternative = "less"
  ))

  expect_equal(result$statistics[[1]]$value, -1.099625, tolerance = 1e-4)
})
