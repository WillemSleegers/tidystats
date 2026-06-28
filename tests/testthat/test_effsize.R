# Compare against the effsize object's own estimate rather than hard-coded
# constants, so the tests stay correct if effsize changes how it computes the
# effect size across versions.

# cohen.d() ---------------------------------------------------------------

test_that("effsize's Cohen's d works", {
  skip_if_not_installed("effsize")
  set.seed(1)

  treatment <- rnorm(100, mean = 10)
  control <- rnorm(100, mean = 12)
  d <- c(treatment, control)
  f <- rep(c("Treatment", "Control"), each = 100)

  es <- effsize::cohen.d(d ~ f)
  result <- tidy_stats(es)

  expect_equal(result$method, "Cohen's d effect size")
  expect_equal(result$statistics[[1]]$value, es$estimate) # d
})

test_that("effsize's Hedges' g works", {
  skip_if_not_installed("effsize")
  set.seed(1)

  treatment <- rnorm(100, mean = 10)
  control <- rnorm(100, mean = 12)
  d <- c(treatment, control)
  f <- rep(c("Treatment", "Control"), each = 100)

  es <- effsize::cohen.d(d ~ f, hedges.correction = TRUE)
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es$estimate) # g
})

test_that("effsize's VDA works", {
  skip_if_not_installed("effsize")
  set.seed(1)

  treatment <- rnorm(100, mean = 10)
  control <- rnorm(100, mean = 12)
  d <- c(treatment, control)
  f <- rep(c("Treatment", "Control"), each = 100)

  es <- effsize::VD.A(d ~ f)
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es$estimate) # A
})

test_that("effsize's Cliff's delta works", {
  skip_if_not_installed("effsize")
  treatment <- c(10, 10, 20, 20, 20, 30, 30, 30, 40, 50)
  control <- c(10, 20, 30, 40, 40, 50)

  es <- effsize::cliff.delta(treatment, control, return.dm = TRUE)
  result <- tidy_stats(es)

  expect_equal(result$statistics[[1]]$value, es$estimate) # delta
})
