# Setup -------------------------------------------------------------------

if (requireNamespace("irr", quietly = TRUE)) library(irr)

# icc() -------------------------------------------------------------------

test_that("irr's icc twoway agreement works", {
  skip_if_not_installed("irr")
  data(anxiety)

  # Compare against the icc() object's own fields rather than hard-coded
  # constants: irr computes the denominator df and p-value differently across
  # versions, so hard-coded values are not portable across CRAN check machines.
  fit <- icc(anxiety, model = "twoway", type = "agreement")
  result <- tidy_stats(fit)

  expect_equal(result$method, "ICC")
  expect_equal(result$statistics[[1]]$value, fit$subjects) # N subjects
  expect_equal(result$statistics[[2]]$value, fit$raters)   # N raters
  expect_equal(result$statistics[[3]]$value, fit$value)    # ICC(A,1)
  expect_equal(result$statistics[[4]]$value, fit$Fvalue)   # statistic
  expect_equal(result$statistics[[5]]$value, fit$df1)      # df numerator
  expect_equal(result$statistics[[6]]$value, fit$df2)      # df denominator
  expect_equal(result$statistics[[7]]$value, fit$p.value)  # p
})

test_that("irr's icc twoway consistency works", {
  skip_if_not_installed("irr")
  set.seed(1)

  r1 <- round(rnorm(20, 10, 4))
  r2 <- round(r1 + 10 + rnorm(20, 0, 2))
  r3 <- round(r1 + 20 + rnorm(20, 0, 2))

  fit <- icc(cbind(r1, r2, r3), "twoway")
  result <- tidy_stats(fit)

  expect_equal(result$method, "ICC")
  expect_equal(result$statistics[[3]]$value, fit$value)   # ICC(C,1)
  expect_equal(result$statistics[[4]]$value, fit$Fvalue)  # statistic
  expect_equal(result$statistics[[5]]$value, fit$df1)     # df numerator
  expect_equal(result$statistics[[6]]$value, fit$df2)     # df denominator
  expect_equal(result$statistics[[7]]$value, fit$p.value) # p
})
