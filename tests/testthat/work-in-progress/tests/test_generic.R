
# Setup -------------------------------------------------------------------

# Load test data
test_results <- read_stats(system.file("test_data/generic.json", 
  package = "tidystats"))

# Set options
tolerance <- 0.001

# Test: Generic tests -----------------------------------------------------

test_that("generic tests works", {
  results <- list()

  # Create generic tests
  generic_ICs <- list(
    statistics = list(
      BIC = 21.21,
      AIC = 478.21
    )
  )
  
  generic_CIs <- list(
    statistics = list(
      CI = list(
        CI_level = .95,
        CI_upper = .64,
        CI_lower = .32
      )
    )
  )
  
  # Add it to the list
  results <- results %>%
    add_stats(generic_ICs, 
      notes = "Wagenmakers (2007) method for calculating Bayes factors") %>%
    add_stats(generic_CIs, notes = "Just some random CIs")
  
  expect_equal(results, test_results, tolerance = tolerance)
})
