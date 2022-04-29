
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(psych)

# Create an empty list 
results <- list()

# alpha() -----------------------------------------------------------------

# Set seed
set.seed(42)

# Run analyses
r4 <- sim.congeneric(loads = c(0.8, 0.7, 0.6, 0.5), low = -3, high = 3)
alpha_r4 <- alpha(r4)

r9 <- sim.hierarchical()
alpha_r9 <- alpha(r9)

two_f <- sim.item(8)
alpha_two_f <- alpha(two_f, keys = c("V3", "V4", "V5", "V6"), n.iter = 500)

cov_two <- cov(two_f)
alpha_cov_two <- alpha(cov_two, check.keys = TRUE)

alpha_two_f_check <- alpha(two_f, check.keys = TRUE) 

items <- sim.congeneric(N = 500, short = FALSE, low = -2, high = 2, 
  categorical = TRUE) 
alpha_a4 <- alpha(items$observed)

# Add stats
results <- results %>%
  add_stats(alpha_r4) %>%
  add_stats(alpha_r9) %>%
  add_stats(alpha_two_f) %>%
  add_stats(alpha_cov_two) %>%
  add_stats(alpha_two_f_check) %>%
  add_stats(alpha_a4) 

# Inspect output
alpha_r4
alpha_r9
alpha_two_f
alpha_cov_two
alpha_two_f_check
alpha_a4

# corr.test() -------------------------------------------------------------

# Run analyses
ct <- corr.test(attitude)
cts <- corr.test(attitude[1:3], attitude[4:6])
cts_single <- corr.test(attitude[1:2], attitude[1:2])
cts_spearman <- corr.test(attitude[1:3], attitude[4:6], method = "spearman")
cts_kendall <- corr.test(attitude[1:3], attitude[4:6], method = "kendall")
sats <- corr.test(sat.act[1:3], sat.act[4:6], adjust = "none")
sats_no_ci <- corr.test(sat.act[1:3], sat.act[4:6], adjust = "none", ci = FALSE)
sats_alpha <- corr.test(sat.act[1:3], sat.act[4:6], adjust = "none", alpha =.1)

# Add stats
results <- results %>%
  add_stats(ct) %>%
  add_stats(cts) %>%
  add_stats(sats) %>%
  add_stats(sats_no_ci) %>%
  add_stats(sats_alpha)

# Inspect output
print(ct, short = FALSE)
cts
print(cts_kendall, short = FALSE)
sats
sats_no_ci
sats_alpha

# mardia() ----------------------------------------------------------------

# Set seed
set.seed(1)

# Get data
x <- matrix(rnorm(1000), ncol = 10)

# Run analyses
mardia_attitude <- mardia(attitude, plot = FALSE)
mardia_x <- mardia(x, plot = FALSE)

# Add stats
results <- results %>%
  add_stats(mardia_attitude) %>%
  add_stats(mardia_x)

mardia_attitude
mardia_x

# Save stats --------------------------------------------------------------

write_stats(results, "tests/testthat/data/psych.json")
