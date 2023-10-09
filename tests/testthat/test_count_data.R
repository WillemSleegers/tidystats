# Setup -------------------------------------------------------------------

expected_statistics <- read_stats("../data/count_data.json")

# count_data() ------------------------------------------------------------

test_that("count data without groups works", {
  model <- count_data(quote_source)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$no_group
  )
})

test_that("count data with one group works", {
  model <- count_data(quote_source, source)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$single_group
  )
})

test_that("count data with two groups works", {
  model <- count_data(quote_source, source, sex, pct = TRUE)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$two_groups
  )
})

test_that("grouped count data with one group works", {
  model <- quote_source |>
    dplyr::group_by(source) |>
    count_data(sex)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$grouped_group
  )
})

test_that("grouped count data with one group without missings works", {
  model <- quote_source |>
    dplyr::group_by(source) |>
    count_data(sex, na.rm = TRUE)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$grouped_group_na_rm
  )
})
