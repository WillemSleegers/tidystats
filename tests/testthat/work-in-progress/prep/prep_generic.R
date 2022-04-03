# Analysis: generic test --------------------------------------------------

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

write_stats(results, "inst/test_data/generic.json")