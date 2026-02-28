# Setup -------------------------------------------------------------------

if (requireNamespace("Hmisc", quietly = TRUE)) library(Hmisc)

# rcorr() -----------------------------------------------------------------

get_pair_stat <- function(result, pair, stat) {
  result$groups[[1]]$groups[[pair]]$statistics[[stat]]$value
}

test_that("rcorr (Pearson) extracts correct correlation values", {
  skip_if_not_installed("Hmisc")
  x <- c(-2, -1, 0, 1, 2)
  y <- c(4, 1, 0, 1, 4)
  z <- c(1, 2, 3, 4, NA)
  v <- c(1, 2, 3, 4, 5)

  result <- tidy_stats(rcorr(cbind(x, y, z, v)))

  expect_equal(result$method, "Correlation")
  expect_equal(length(result$groups[[1]]$groups), 6) # 4 choose 2

  # x vs y: r, n, p
  expect_equal(get_pair_stat(result, 1, 1), 0,         tolerance = 1e-6)
  expect_equal(get_pair_stat(result, 1, 2), 5,         tolerance = 1e-6)
  expect_equal(get_pair_stat(result, 1, 3), 1,         tolerance = 1e-6)

  # x vs z: r=1, n=4, p=0
  expect_equal(get_pair_stat(result, 2, 1), 1,         tolerance = 1e-6)
  expect_equal(get_pair_stat(result, 2, 2), 4,         tolerance = 1e-6)
  expect_equal(get_pair_stat(result, 2, 3), 0,         tolerance = 1e-6)

  # y vs z: r=-0.745356, n=4, p=0.254644
  expect_equal(get_pair_stat(result, 3, 1), -0.745356, tolerance = 1e-4)
  expect_equal(get_pair_stat(result, 3, 2), 4,         tolerance = 1e-6)
  expect_equal(get_pair_stat(result, 3, 3), 0.254644,  tolerance = 1e-4)

  # x vs v: r=1, n=5, p=0
  expect_equal(get_pair_stat(result, 4, 1), 1,         tolerance = 1e-6)

  # y vs v: r=0, n=5, p=1
  expect_equal(get_pair_stat(result, 5, 1), 0,         tolerance = 1e-6)
  expect_equal(get_pair_stat(result, 5, 3), 1,         tolerance = 1e-6)

  # z vs v: r=1, n=4, p=0
  expect_equal(get_pair_stat(result, 6, 1), 1,         tolerance = 1e-6)
  expect_equal(get_pair_stat(result, 6, 2), 4,         tolerance = 1e-6)
})

test_that("rcorr (Spearman) extracts correct correlation values", {
  skip_if_not_installed("Hmisc")
  x <- c(-2, -1, 0, 1, 2)
  y <- c(4, 1, 0, 1, 4)
  z <- c(1, 2, 3, 4, NA)
  v <- c(1, 2, 3, 4, 5)

  result <- tidy_stats(rcorr(cbind(x, y, z, v), type = "spearman"))

  expect_equal(result$method, "Correlation")
  expect_equal(length(result$groups[[1]]$groups), 6)

  # x vs y: r=0, n=5, p=1
  expect_equal(get_pair_stat(result, 1, 1), 0,          tolerance = 1e-6)
  expect_equal(get_pair_stat(result, 1, 3), 1,          tolerance = 1e-6)

  # y vs z: r=-0.6324555, n=4, p=0.3675445
  expect_equal(get_pair_stat(result, 3, 1), -0.6324555, tolerance = 1e-4)
  expect_equal(get_pair_stat(result, 3, 2), 4,          tolerance = 1e-6)
  expect_equal(get_pair_stat(result, 3, 3), 0.3675445,  tolerance = 1e-4)

  # x vs v: r=1
  expect_equal(get_pair_stat(result, 4, 1), 1,          tolerance = 1e-6)
})
