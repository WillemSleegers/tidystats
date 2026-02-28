# count_data() ------------------------------------------------------------

test_that("count data without groups works", {
  result <- tidy_stats(count_data(quote_source))

  expect_equal(result$method, "Counts")
  expect_equal(result$statistics[[1]]$value, 6343, tolerance = 1e-6) # n
  expect_equal(result$statistics[[2]]$value, 1,    tolerance = 1e-6) # proportion
})

test_that("count data with one group works", {
  result <- tidy_stats(count_data(quote_source, source))

  source_group <- result$groups[[1]]
  expect_equal(source_group$name, "source")
  expect_equal(source_group$groups[[1]]$name, "Bin Laden")
  expect_equal(source_group$groups[[1]]$statistics[[1]]$value, 3101,     tolerance = 1e-6) # n
  expect_equal(source_group$groups[[1]]$statistics[[2]]$value, 0.4888854, tolerance = 1e-4) # prop
  expect_equal(source_group$groups[[2]]$name, "Washington")
})

test_that("count data with two groups works", {
  result <- tidy_stats(count_data(quote_source, source, sex, pct = TRUE))

  expect_equal(result$method, "Counts")
  expect_true(length(result$groups) > 0)
})

test_that("grouped count data with one group works", {
  skip_if_not_installed("dplyr")
  result <- tidy_stats(
    quote_source |>
      dplyr::group_by(source) |>
      count_data(sex)
  )

  expect_equal(result$method, "Counts")
  expect_true(length(result$groups) > 0)
})

test_that("grouped count data with one group without missings works", {
  skip_if_not_installed("dplyr")
  result <- tidy_stats(
    quote_source |>
      dplyr::group_by(source) |>
      count_data(sex, na.rm = TRUE)
  )

  expect_equal(result$method, "Counts")
})

# No columns --------------------------------------------------------------

test_that("no columns returns total row count", {
  df <- data.frame(x = c("a", "b", "a"))
  result <- count_data(df)

  expect_equal(nrow(result), 1)
  expect_equal(result$n, 3)
  expect_equal(result$prop, 1)
})

# Basic correctness -------------------------------------------------------

test_that("counts are correct", {
  df <- data.frame(x = c("a", "b", "a", "a"))
  result <- count_data(df, x)

  expect_equal(result$n[result$x == "a"], 3)
  expect_equal(result$n[result$x == "b"], 1)
})

test_that("multiple columns produce correct combinations", {
  df <- data.frame(
    x = c("a", "a", "b", "b"),
    y = c("1", "2", "1", "1")
  )
  result <- count_data(df, x, y)

  expect_equal(nrow(result), 3) # a-1, a-2, b-1
})

test_that("grouped counts and proportions are correct", {
  skip_if_not_installed("dplyr")
  df <- data.frame(
    group = c("a", "a", "b", "b"),
    x     = c("1", "1", "1", "2")
  )
  result <- df |>
    dplyr::group_by(group) |>
    count_data(x)

  expect_equal(result$n[result$group == "a" & result$x == "1"], 2)
  expect_equal(result$prop[result$group == "a" & result$x == "1"], 1)
  expect_equal(result$n[result$group == "b" & result$x == "1"], 1)
  expect_equal(result$prop[result$group == "b" & result$x == "1"], 0.5)
})

# Output structure --------------------------------------------------------

test_that("output has tidystats_counts class", {
  result <- count_data(quote_source, source)
  expect_true(inherits(result, "tidystats_counts"))
})

test_that("prop column is present by default, not pct", {
  result <- count_data(quote_source, source)
  expect_true("prop" %in% names(result))
  expect_false("pct" %in% names(result))
})

test_that("pct column is present when pct = TRUE, not prop", {
  result <- count_data(quote_source, source, pct = TRUE)
  expect_true("pct" %in% names(result))
  expect_false("prop" %in% names(result))
})

# na.rm -------------------------------------------------------------------

test_that("na.rm removes missing observations", {
  df <- data.frame(x = c("a", NA, "a"))
  result <- count_data(df, x, na.rm = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$n, 2)
})

# Proportion and percentage calculations ----------------------------------

test_that("ungrouped proportions sum to 1", {
  result <- count_data(quote_source, source)
  expect_equal(sum(result$prop), 1)
})

test_that("grouped proportions sum to 1 within each group", {
  skip_if_not_installed("dplyr")
  result <- quote_source |>
    dplyr::group_by(source) |>
    count_data(sex)

  group_sums <- tapply(result$prop, result$source, sum)
  expect_true(all(abs(group_sums - 1) < 1e-10))
})

test_that("ungrouped percentages sum to 100", {
  result <- count_data(quote_source, source, pct = TRUE)
  expect_equal(sum(result$pct), 100)
})

test_that("grouped percentages sum to 100 within each group", {
  skip_if_not_installed("dplyr")
  result <- quote_source |>
    dplyr::group_by(source) |>
    count_data(sex, pct = TRUE)

  group_sums <- tapply(result$pct, result$source, sum)
  expect_true(all(abs(group_sums - 100) < 1e-10))
})

# NA handling -------------------------------------------------------------

test_that("NA is counted as a separate category", {
  df <- data.frame(x = c("a", NA, "a", NA))
  result <- count_data(df, x)

  expect_equal(nrow(result), 2)
  expect_equal(result$n[is.na(result$x)], 2)
  expect_equal(result$n[!is.na(result$x)], 2)
})

test_that("NA and the string 'NA' are counted separately", {
  df <- data.frame(x = c("a", NA, "NA", "a"))
  result <- count_data(df, x)

  expect_equal(nrow(result), 3)
})
