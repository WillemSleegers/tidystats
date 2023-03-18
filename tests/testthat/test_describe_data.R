# Setup -------------------------------------------------------------------

path <- system.file("tests/data/describe_data.json", package = "tidystats")
expected_statistics <- read_stats(path)

# describe_data() ---------------------------------------------------------

test_that("describe data works", {
  model <- describe_data(quote_source, response)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$single_var
  )
})

test_that("describe data with one group works", {
  model <- quote_source |>
    dplyr::group_by(source) |>
    describe_data(response)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$single_var_w_group
  )
})

test_that("multiple vars describe data works", {
  model <- describe_data(quote_source, response, age)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$multiple_vars
  )
})

test_that("describe data with multiple groups works", {
  model <- quote_source |>
    dplyr::group_by(source, sex) |>
    describe_data(response)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$single_var_w_groups
  )
})

test_that("describe data with multiple groups without missings works", {
  model <- quote_source |>
    dplyr::group_by(source, sex) |>
    describe_data(response, na.rm = FALSE)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$single_var_w_groups_wo_na
  )
})

test_that("describe data with multiple vars and a group works", {
  model <- quote_source |>
    dplyr::group_by(source) |>
    describe_data(response, age)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$multiple_vars_w_group
  )
})

test_that("describe data with subset works", {
  model <- describe_data(quote_source, response, short = TRUE)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$single_var_subset
  )
})
