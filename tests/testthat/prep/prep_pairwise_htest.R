
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
results <- list()

# pairwise.t.test()  --------------------------------------------------------------

# Get data
Month <- factor(airquality$Month, labels = month.abb[5:9])

# Run analysis
# pairwise_t_test = pairwise.t.test(airquality$Ozone[1:45], Month[1:45])
pairwise_t_test = pairwise.t.test(airquality$Ozone, Month)

pairwise_t_test_paired = pairwise.t.test(c(1,2,3,1,2,4), c(1,1,2,2,3,3), paired = TRUE)

pairwise_t_test_nonpooled = pairwise.t.test(airquality$Ozone,
  Month,
  p.adjust.method = "bonf",
  pool.sd = FALSE)

# Add stats
results <- results %>%
  add_stats(pairwise_t_test) %>%
  add_stats(pairwise_t_test_paired) %>%
  add_stats(pairwise_t_test_nonpooled)

# Inspect output
pairwise_t_test
pairwise_t_test_paired
pairwise_t_test_nonpooled


# pairwise.prop.test()  --------------------------------------------------------------

# Get data
smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )

# Run analysis
pairwise_prop_test = pairwise.prop.test(smokers, patients)

# Add stats
results <- add_stats(results, pairwise_prop_test)

# Inspect output
pairwise_prop_test

# pairwise.wilcox.test()  --------------------------------------------------------------

# Run analysis
pairwise_wilcox_test = pairwise.wilcox.test(c(1,2,3,4,5,6,7,8,9,11), c(1,1,1,1,1,2,2,2,2,2))
pairwise_wilcox_test_paired = pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
  p.adjust.method = "BH", paired = TRUE)

# Add stats
results <- results %>%
  add_stats(pairwise_wilcox_test) %>%
  add_stats(pairwise_wilcox_test_paired)

# Inspect output
pairwise_wilcox_test
pairwise_wilcox_test_paired


# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/pairwise_htest.json")

# results = list()
# write_stats(results, "tests/testthat/data/0temporary.json")
