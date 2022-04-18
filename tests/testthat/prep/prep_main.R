
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
results <- list()

# t.test() ----------------------------------------------------------------

sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)

# lm() --------------------------------------------------------------------

D9 <- tibble(
  group = gl(2, 10, 20, labels = c("Ctl", "Trt")),
  weight = c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14, 4.81, 
    4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
)

lm_D9 <- lm(weight ~ group, data = D9)

# aov() -------------------------------------------------------------------

npk_aov <- aov(yield ~ block + N*P*K, npk)

# add_stats() ---------------------------------------------------------------

results <- results %>%
  add_stats(sleep_test, type = "primary") %>%
  add_stats(lm_D9, preregistered = FALSE) %>%
  add_stats(npk_aov, notes = "An ANOVA example")

# Inspect output ----------------------------------------------------------

sleep_test
lm_D9
npk_aov

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/main.json")

# Cleanup -----------------------------------------------------------------

rm(sleep_test, D9, lm_D9, npk_aov, df, results)
