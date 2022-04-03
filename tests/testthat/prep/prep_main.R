
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
results <- list()

# Run analyses ------------------------------------------------------------

# Conduct three different analyses
# t-test:
sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)

# lm:
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm_D9 <- lm(weight ~ group)

# ANOVA:
npk_aov <- aov(yield ~ block + N*P*K, npk)

# Add stats ---------------------------------------------------------------

results <- results %>%
  add_stats(sleep_test, type = "primary") %>%
  add_stats(lm_D9, preregistered = FALSE) %>%
  add_stats(npk_aov, notes = "An ANOVA example")

sleep_test
lm_D9
npk_aov

# Convert to data frame ---------------------------------------------------

df <- tidy_stats_to_data_frame(results)

# Write files -------------------------------------------------------------

write_stats(results, "tests/testthat/data/main.json")
write_csv(df, "tests/testthat/data/main_df.csv")
