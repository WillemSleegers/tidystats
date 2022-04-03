
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(psych)

# Create an empty list 
results <- list()

# alpha -------------------------------------------------------------------

# Set seed
set.seed(42)

# Four congeneric measures
r4 <- sim.congeneric()
alpha_r4 <- alpha(r4)

# Nine hierarchical measures (Should actually use omega)
r9 <- sim.hierarchical()
alpha_r9 <- alpha(r9)

# Examples of two independent factors that produce reasonable alphas
# This is a case where alpha is a poor indicator of unidimensionality
two_f <- sim.item(8)

# Specify which items to reverse key by name
alpha_two_f <- alpha(two_f, keys = c("V3", "V4", "V5", "V6"))

cov_two <- cov(two_f)
alpha_cov_two <- alpha(cov_two, check.keys = TRUE)

# Automatic reversal base upon first component
alpha_two_f_check <- alpha(two_f, check.keys = TRUE) #note that the median is much less than the average R
#this suggests (correctly) that the 1 factor model is probably wrong 
#an example with discrete item responses  -- show the frequencies

# Simulate 500 responses to 4 discrete items with 5 categories
items <- sim.congeneric(N = 500, short = FALSE, low = -2, high = 2, 
  categorical = TRUE) 

alpha_a4 <- alpha(items$observed)  #item response analysis of congeneric measures
alpha_a4
#summary just gives Alpha
summary(alpha_a4)

# Tidy stats
temp <- tidy_stats(alpha_r4)

# Add stats
results <- results %>%
  add_stats(alpha_r4)

# corr.test ---------------------------------------------------------------

# Example 1
ct <- corr.test(attitude)
ct

# Example 2
cts <- corr.test(attitude[1:3], attitude[4:6])
cts

# Example 3
sats <- corr.test(sat.act[1:3], sat.act[4:6], adjust = "none")

# Example 4
sats_no_ci <- corr.test(sat.act[1:3], sat.act[4:6], adjust = "none", ci = FALSE)

# Example 5
sats_alpha <- corr.test(sat.act[1:3], sat.act[4:6], adjust = "none", alpha =.1)

# Tidy stats
temp <- tidy_stats(ct)
temp <- tidy_stats(cts)
temp <- tidy_stats(sats)

# Add stats
results <- results %>%
  add_stats(ct) %>%
  add_stats(cts) %>%
  add_stats(sats) %>%
  add_stats(sats_no_ci) %>%
  add_stats(sats_alpha)

# Save stats --------------------------------------------------------------

write_stats(results, "inst/test_data/psych.json")
