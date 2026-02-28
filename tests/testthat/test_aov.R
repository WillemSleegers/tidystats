# aov() -------------------------------------------------------------------

test_that("aov works", {
  result <- tidy_stats(aov(yield ~ block + N * P * K, npk))

  expect_equal(result$method, "ANOVA")

  terms <- result$groups[[1]]$groups
  expect_equal(terms[[1]]$name, "block")
  expect_equal(terms[[1]]$statistics[[3]]$value, 4.446666,  tolerance = 1e-4) # F
  expect_equal(terms[[1]]$statistics[[6]]$value, 0.01593879, tolerance = 1e-4) # p
  expect_equal(terms[[2]]$name, "N")
  expect_equal(terms[[2]]$statistics[[3]]$value, 12.25873,   tolerance = 1e-4) # F
  expect_equal(terms[[2]]$statistics[[6]]$value, 0.004371812, tolerance = 1e-4) # p
  expect_equal(terms[[4]]$name, "K")
  expect_equal(terms[[4]]$statistics[[3]]$value, 6.165689,  tolerance = 1e-4) # F
  expect_equal(terms[[4]]$statistics[[6]]$value, 0.02879505, tolerance = 1e-4) # p
})

test_that("aov order works", {
  result <- tidy_stats(aov(terms(yield ~ block + N * P + K, keep.order = TRUE), npk))

  terms <- result$groups[[1]]$groups
  expect_equal(terms[[1]]$name, "block")
  expect_equal(terms[[1]]$statistics[[3]]$value, 4.391098, tolerance = 1e-4) # F
  expect_equal(terms[[2]]$name, "N")
  expect_equal(terms[[2]]$statistics[[3]]$value, 12.10554, tolerance = 1e-4) # F
})

test_that("aov error works", {
  result <- tidy_stats(aov(yield ~ N * P * K + Error(block), npk))

  error_terms <- result$groups[[1]]$groups

  # Error: block contains N:P:K
  block_terms <- error_terms[[1]]$groups[[1]]$groups
  expect_equal(block_terms[[1]]$name, "N:P:K")
  expect_equal(block_terms[[1]]$statistics[[3]]$value, 0.4832187, tolerance = 1e-4) # F
  expect_equal(block_terms[[1]]$statistics[[6]]$value, 0.5252361, tolerance = 1e-4) # p

  # Error: Within contains N, P, K
  within_terms <- error_terms[[2]]$groups[[1]]$groups
  expect_equal(within_terms[[1]]$name, "N")
  expect_equal(within_terms[[1]]$statistics[[3]]$value, 12.25873,    tolerance = 1e-4) # F
  expect_equal(within_terms[[1]]$statistics[[6]]$value, 0.004371812, tolerance = 1e-4) # p
  expect_equal(within_terms[[3]]$name, "K")
  expect_equal(within_terms[[3]]$statistics[[3]]$value, 6.165689,  tolerance = 1e-4) # F
  expect_equal(within_terms[[3]]$statistics[[6]]$value, 0.02879505, tolerance = 1e-4) # p
})
