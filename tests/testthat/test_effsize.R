# cohen.d() ---------------------------------------------------------------

test_that("effsize's Cohen's d works", {
  skip_if_not_installed("effsize")
  set.seed(1)

  treatment <- rnorm(100, mean = 10)
  control <- rnorm(100, mean = 12)
  d <- c(treatment, control)
  f <- rep(c("Treatment", "Control"), each = 100)

  result <- tidy_stats(effsize::cohen.d(d ~ f))

  expect_equal(result$method, "Cohen's d effect size")
  expect_equal(result$statistics[[1]]$value, 1.99598, tolerance = 1e-4) # d
})

test_that("effsize's Hedges' g works", {
  skip_if_not_installed("effsize")
  set.seed(1)

  treatment <- rnorm(100, mean = 10)
  control <- rnorm(100, mean = 12)
  d <- c(treatment, control)
  f <- rep(c("Treatment", "Control"), each = 100)

  result <- tidy_stats(effsize::cohen.d(d ~ f, hedges.correction = TRUE))

  expect_equal(result$statistics[[1]]$value, 1.988409, tolerance = 1e-4) # g
})

test_that("effsize's VDA works", {
  skip_if_not_installed("effsize")
  set.seed(1)

  treatment <- rnorm(100, mean = 10)
  control <- rnorm(100, mean = 12)
  d <- c(treatment, control)
  f <- rep(c("Treatment", "Control"), each = 100)

  result <- tidy_stats(effsize::VD.A(d ~ f))

  expect_equal(result$statistics[[1]]$value, 0.9286, tolerance = 1e-4) # A
})

test_that("effsize's Cliff's delta works", {
  skip_if_not_installed("effsize")
  treatment <- c(10, 10, 20, 20, 20, 30, 30, 30, 40, 50)
  control <- c(10, 20, 30, 40, 40, 50)

  result <- tidy_stats(effsize::cliff.delta(treatment, control, return.dm = TRUE))

  expect_equal(result$statistics[[1]]$value, -0.25, tolerance = 1e-4) # delta
})
