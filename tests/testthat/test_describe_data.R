# describe_data() ---------------------------------------------------------

test_that("describe data works", {
  result <- tidy_stats(describe_data(quote_source, response))

  expect_equal(result$method, "Descriptives")
  expect_equal(result$name, "response")
  expect_equal(result$statistics[[1]]$value, 18,       tolerance = 1e-6) # missing
  expect_equal(result$statistics[[2]]$value, 6325,     tolerance = 1e-6) # N
  expect_equal(result$statistics[[3]]$value, 5.588617, tolerance = 1e-4) # mean
  expect_equal(result$statistics[[4]]$value, 2.189027, tolerance = 1e-4) # SD
})

test_that("describe data with one group works", {
  result <- tidy_stats(describe_data(quote_source, response, by = "source"))

  source_group <- result$groups[[1]]
  expect_equal(source_group$name, "source")

  bin_laden <- source_group$groups[[1]]
  expect_equal(bin_laden$name, "Bin Laden")
  expect_equal(bin_laden$statistics[[2]]$value, 3083,     tolerance = 1e-6) # N
  expect_equal(bin_laden$statistics[[3]]$value, 5.232241, tolerance = 1e-4) # mean

  washington <- source_group$groups[[2]]
  expect_equal(washington$name, "Washington")
  expect_equal(washington$statistics[[2]]$value, 3242,     tolerance = 1e-6) # N
  expect_equal(washington$statistics[[3]]$value, 5.927514, tolerance = 1e-4) # mean
})

test_that("multiple vars describe data works", {
  result <- tidy_stats(describe_data(quote_source, response, age))

  expect_true(length(result$groups) == 2)
  age_group <- result$groups[[which(sapply(result$groups, function(g) g$name == "age"))]]
  resp_group <- result$groups[[which(sapply(result$groups, function(g) g$name == "response"))]]

  expect_equal(age_group$statistics[[3]]$value, 25.97598, tolerance = 1e-4) # mean
  expect_equal(resp_group$statistics[[3]]$value, 5.588617, tolerance = 1e-4) # mean
})

test_that("describe data with multiple groups works", {
  result <- tidy_stats(describe_data(quote_source, response, by = c("source", "sex")))

  expect_equal(result$name, "response")
  source_group <- result$groups[[1]]
  expect_equal(source_group$name, "source")
  expect_true(length(source_group$groups) > 0)
})

test_that("describe data with multiple groups without missings works", {
  result <- tidy_stats(describe_data(quote_source, response, by = c("source", "sex"), na.rm = FALSE))

  expect_equal(result$method, "Descriptives")
})

test_that("describe data with multiple vars and a group works", {
  result <- tidy_stats(describe_data(quote_source, response, age, by = "source"))

  expect_true(length(result$groups) >= 2)
})

test_that("describe data with subset works", {
  result <- tidy_stats(describe_data(quote_source, response, short = TRUE))

  expect_equal(result$method, "Descriptives")
  expect_equal(result$name, "response")
  expect_equal(result$statistics[[1]]$value, 6325, tolerance = 1e-6) # N
})
