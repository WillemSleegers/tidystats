# custom_stats() ----------------------------------------------------------

test_that("BF stats works", {
  lm1 <- lm(Fertility ~ ., data = swiss)
  lm2 <- update(lm1, . ~ . - Examination)

  BF10 <- 1 / exp((BIC(lm2) - BIC(lm1)) / 2)

  BF_stats <- custom_stats(
    method = "BF BIC method",
    statistics = c(
      custom_stat(name = "BF", value = BF10, subscript = "10"),
      custom_stat(name = "BF", value = 1 / BF10, subscript = "01")
    )
  )

  statistics <- add_stats(
    list = list(),
    output = BF_stats,
    notes = "Wagenmakers (2007) method for calculating Bayes factors"
  )

  expect_equal(statistics$BF_stats$method, "BF BIC method")
  expect_equal(statistics$BF_stats$statistics[[1]]$value, 3.820709,  tolerance = 1e-4) # BF10
  expect_equal(statistics$BF_stats$statistics[[2]]$value, 0.2617315, tolerance = 1e-4) # BF01
})
