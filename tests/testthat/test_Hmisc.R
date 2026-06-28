# Setup -------------------------------------------------------------------

if (requireNamespace("Hmisc", quietly = TRUE)) library(Hmisc)

# Compare against the rcorr object's own r/n/P matrices (looked up by variable
# name) rather than hard-coded constants, so the tests verify tidy_stats's
# extraction independent of the Hmisc version.

# rcorr() -----------------------------------------------------------------

check_pairs <- function(result, rc) {
  groups <- result$groups[[1]]$groups
  for (g in groups) {
    n1 <- g$names[[1]]$name
    n2 <- g$names[[2]]$name
    expect_equal(g$statistics[[1]]$value, rc$r[n1, n2]) # r
    expect_equal(g$statistics[[2]]$value, rc$n[n1, n2]) # n
    expect_equal(g$statistics[[3]]$value, rc$P[n1, n2]) # p
  }
}

test_that("rcorr (Pearson) extracts correct correlation values", {
  skip_if_not_installed("Hmisc")
  x <- c(-2, -1, 0, 1, 2)
  y <- c(4, 1, 0, 1, 4)
  z <- c(1, 2, 3, 4, NA)
  v <- c(1, 2, 3, 4, 5)

  rc <- rcorr(cbind(x, y, z, v))
  result <- tidy_stats(rc)

  expect_equal(result$method, "Correlation")
  expect_equal(length(result$groups[[1]]$groups), 6) # 4 choose 2
  check_pairs(result, rc)
})

test_that("rcorr (Spearman) extracts correct correlation values", {
  skip_if_not_installed("Hmisc")
  x <- c(-2, -1, 0, 1, 2)
  y <- c(4, 1, 0, 1, 4)
  z <- c(1, 2, 3, 4, NA)
  v <- c(1, 2, 3, 4, 5)

  rc <- rcorr(cbind(x, y, z, v), type = "spearman")
  result <- tidy_stats(rc)

  expect_equal(result$method, "Correlation")
  expect_equal(length(result$groups[[1]]$groups), 6)
  check_pairs(result, rc)
})
