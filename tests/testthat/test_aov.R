# Setup -------------------------------------------------------------------

# Load test data
path <- system.file("tests/data/aov.json", package = "tidystats")
expected_statistics <- read_stats(path)

# aov() -------------------------------------------------------------------

test_that("aov works", {
  model <- aov(yield ~ block + N * P * K, npk)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov
  )
})

test_that("aov order works", {
  model <- aov(terms(yield ~ block + N * P + K, keep.order = TRUE), npk)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_order
  )
})

test_that("aov error works", {
  model <- aov(yield ~ N * P * K + Error(block), npk)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_error
  )
})
