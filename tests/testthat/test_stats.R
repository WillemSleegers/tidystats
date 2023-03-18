# Setup -------------------------------------------------------------------

path <- system.file("tests/data/stats.json", package = "tidystats")
expected_statistics <- read_stats(path)

# stat() and stats() ------------------------------------------------------

test_that("BF stats works", {
  statistics <- list()

  lm1 <- lm(Fertility ~ ., data = swiss)
  lm2 <- update(lm1, . ~ . - Examination)

  BF10 <- 1 / exp((BIC(lm2) - BIC(lm1)) / 2)

  BF_stats <- stats(
    method = "BF BIC method",
    statistics = c(
      stat(name = "BF", value = BF10, subscript = "10"),
      stat(name = "BF", value = 1 / BF10, subscript = "01")
    )
  )

  statistics <- add_stats(
    list = statistics,
    output = BF_stats,
    notes = "Wagenmakers (2007) method for calculating Bayes factors"
  )
  statistics$BF_stats$statistics[[1]]
  expected_statistics$BF_stats$statistics[[1]]

  expect_equivalent(
    statistics$BF_stats,
    expected_statistics$BF_stats,
    tolerance = 0.00001
  )
})
