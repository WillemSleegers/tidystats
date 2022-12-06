
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
statistics <- list()

# lm() --------------------------------------------------------------------

# Get data
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
weight <- c(ctl, trt)

# Run analyses
lm <- lm(weight ~ group)
lm_wo_intercept <- lm(weight ~ group - 1) # omitting intercept

# Add stats
statistics <- statistics %>%
  add_stats(lm) %>%
  add_stats(lm_wo_intercept)

# Inspect output
summary(lm)
summary(lm_wo_intercept)

# anova() -----------------------------------------------------------------

# Run analyses
fit <- lm(sr ~ ., data = LifeCycleSavings)
fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
fit1 <- update(fit0, . ~ . + pop15)
fit2 <- update(fit1, . ~ . + pop75)
fit3 <- update(fit2, . ~ . + dpi)
fit4 <- update(fit3, . ~ . + ddpi)

anova_lm <- anova(fit)
anova_lm_fits <- anova(fit0, fit1, fit2, fit3, fit4, test = "F")
anova_lm_order <- anova(fit4, fit2, fit0, test = "F")
anova_lm_chisq <- anova(fit4, fit2, fit0, test = "Chisq")
anova_lm_cp <- anova(fit4, fit2, fit0, test = "Cp")

# Add stats
statistics <- statistics %>%
  add_stats(anova_lm) %>%
  add_stats(anova_lm_fits) %>%
  add_stats(anova_lm_order) %>%
  add_stats(anova_lm_chisq) %>%
  add_stats(anova_lm_cp)

# Inspect output
anova_lm
anova_lm_fits
anova_lm_order
anova_lm_chisq
anova_lm_cp

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/testthat/data/lm.json")

# Cleanup -----------------------------------------------------------------

rm(
  df, lm, lm_wo_intercept, statistics, ctl, group, trt, weight, 
  fit, fit0, fit1, fit2, fit3, fit4, anova_lm, anova_lm_chisq, anova_lm_cp,
  anova_lm_fits, anova_lm_order
)
