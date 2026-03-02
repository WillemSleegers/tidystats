# Setup -------------------------------------------------------------------

statistics <- list()

# pairwise.t.test() -------------------------------------------------------

Month <- factor(airquality$Month, labels = month.abb[5:9])

pairwise_t_test <- pairwise.t.test(airquality$Ozone, Month)
pairwise_t_test_nonpooled <- pairwise.t.test(
  airquality$Ozone,
  Month,
  p.adjust.method = "bonf",
  pool.sd = FALSE
)
pairwise_t_test_paired <- pairwise.t.test(
  c(1, 2, 3, 1, 2, 4),
  c(1, 1, 2, 2, 3, 3),
  paired = TRUE
)

statistics <- statistics |>
  add_stats(pairwise_t_test) |>
  add_stats(pairwise_t_test_paired) |>
  add_stats(pairwise_t_test_nonpooled)

pairwise_t_test
pairwise_t_test_paired
pairwise_t_test_nonpooled

# pairwise.prop.test() ----------------------------------------------------

smokers <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)

pairwise_prop_test <- pairwise.prop.test(smokers, patients)

statistics <- add_stats(statistics, pairwise_prop_test)

pairwise_prop_test

# pairwise.wilcox.test() --------------------------------------------------

pairwise_wilcox_test <- pairwise.wilcox.test(
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11),
  c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
)
pairwise_wilcox_test_paired <- pairwise.wilcox.test(
  PlantGrowth$weight,
  PlantGrowth$group,
  p.adjust.method = "BH",
  paired = TRUE
)

statistics <- statistics |>
  add_stats(pairwise_wilcox_test) |>
  add_stats(pairwise_wilcox_test_paired)

pairwise_wilcox_test
pairwise_wilcox_test_paired

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/pairwise_htest.json")

# Cleanup -----------------------------------------------------------------

rm(
  Month, pairwise_prop_test, pairwise_t_test, pairwise_t_test_nonpooled,
  pairwise_t_test_paired, pairwise_wilcox_test, pairwise_wilcox_test_paired,
  patients, smokers, statistics
)
