
# Setup -------------------------------------------------------------------

# Load test data
path <- system.file("tests/testthat/data/generic.json", package = "tidystats")
expected_statistics <- read_stats(path)

# Generic statistics ------------------------------------------------------

test_that("generic statistics works", {
  statistics <- list()

  BIC_AIC <- list(
    name = "BIC/AIC",
    statistics = list(
      list(name = "BIC", value = 21.21),
      list(name = "AIC", value = 478.21)
    )
  )
  
  statistics <- add_stats(
    statistics, 
    BIC_AIC, 
    notes = "Wagenmakers (2007) method for calculating Bayes factors"
  )
  
  expect_equal(statistics, expected_statistics)
})
