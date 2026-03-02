# Setup -------------------------------------------------------------------

library(irr)

statistics <- list()

# icc() -------------------------------------------------------------------

set.seed(1)

data(anxiety)

r1 <- round(rnorm(20, 10, 4))
r2 <- round(r1 + 10 + rnorm(20, 0, 2))
r3 <- round(r1 + 20 + rnorm(20, 0, 2))

ICC_anxiety <- icc(anxiety, model = "twoway", type = "agreement")
ICC_high_consistency <- icc(cbind(r1, r2, r3), "twoway")
ICC_low_agreement <- icc(cbind(r1, r2, r3), "twoway", "agreement")

statistics <- statistics |>
  add_stats(ICC_anxiety) |>
  add_stats(ICC_high_consistency) |>
  add_stats(ICC_low_agreement)

ICC_anxiety
ICC_high_consistency
ICC_low_agreement

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/irr.json")

# Cleanup -----------------------------------------------------------------

rm(
  anxiety, ICC_anxiety, ICC_high_consistency, ICC_low_agreement, statistics, df,
  r1, r2, r3
)
