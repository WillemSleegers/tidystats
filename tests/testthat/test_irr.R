# Setup -------------------------------------------------------------------

library(irr)

expected_statistics <- read_stats("../data/irr.json")

# aov() -------------------------------------------------------------------

test_that("irr's icc twoway agreement works", {
  data(anxiety)

  model <- icc(anxiety, model = "twoway", type = "agreement")

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$ICC_anxiety
  )
})

test_that("irrs's icc twoway consistency works", {
  set.seed(1)

  r1 <- round(rnorm(20, 10, 4))
  r2 <- round(r1 + 10 + rnorm(20, 0, 2))
  r3 <- round(r1 + 20 + rnorm(20, 0, 2))

  model <- icc(cbind(r1, r2, r3), "twoway")

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$ICC_high_consistency
  )
})
