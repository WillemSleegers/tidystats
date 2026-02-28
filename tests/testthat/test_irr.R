# Setup -------------------------------------------------------------------

if (requireNamespace("irr", quietly = TRUE)) library(irr)

# icc() -------------------------------------------------------------------

test_that("irr's icc twoway agreement works", {
  skip_if_not_installed("irr")
  data(anxiety)

  result <- tidy_stats(icc(anxiety, model = "twoway", type = "agreement"))

  expect_equal(result$method, "ICC")
  expect_equal(result$statistics[[1]]$value, 20,          tolerance = 1e-6) # N subjects
  expect_equal(result$statistics[[2]]$value, 3,           tolerance = 1e-6) # N raters
  expect_equal(result$statistics[[3]]$value, 0.1979983,   tolerance = 1e-4) # ICC(A,1)
  expect_equal(result$statistics[[4]]$value, 1.826772,    tolerance = 1e-4) # statistic
  expect_equal(result$statistics[[5]]$value, 19,          tolerance = 1e-6) # df numerator
  expect_equal(result$statistics[[6]]$value, 39.74604,    tolerance = 1e-3) # df denominator
  expect_equal(result$statistics[[7]]$value, 0.05426568,  tolerance = 1e-4) # p
})

test_that("irr's icc twoway consistency works", {
  skip_if_not_installed("irr")
  set.seed(1)

  r1 <- round(rnorm(20, 10, 4))
  r2 <- round(r1 + 10 + rnorm(20, 0, 2))
  r3 <- round(r1 + 20 + rnorm(20, 0, 2))

  result <- tidy_stats(icc(cbind(r1, r2, r3), "twoway"))

  expect_equal(result$method, "ICC")
  expect_equal(result$statistics[[3]]$value, 0.8456151,    tolerance = 1e-4) # ICC(C,1)
  expect_equal(result$statistics[[4]]$value, 17.43195,     tolerance = 1e-3) # statistic
  expect_equal(result$statistics[[5]]$value, 19,           tolerance = 1e-6) # df numerator
  expect_equal(result$statistics[[6]]$value, 38,           tolerance = 1e-6) # df denominator
  expect_equal(result$statistics[[7]]$value, 2.852714e-13, tolerance = 1e-4) # p
})
