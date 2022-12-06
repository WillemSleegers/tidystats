
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lme4)

# Create an empty list
statistics <- list()

# lmer() ------------------------------------------------------------------

# Run analyses
lme4 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
lme4_ML <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy, REML = FALSE)
lme4_slopes <- lmer(
  Reaction ~ Days + (Days | Subject), 
  sleepstudy, 
  REML = FALSE
)

# Add stats
statistics <- statistics %>%
  add_stats(lme4) %>%
  add_stats(lme4_ML) %>%
  add_stats(lme4_slopes) 

# Inspect output
summary(lme4)
summary(lme4_ML)
summary(lme4_slopes)

# anova() -----------------------------------------------------------------

# Run analyses
anova_lme4 <- anova(lme4)
anova_models <- anova(lme4, lme4_slopes)

# Add stats
statistics <- statistics %>%
  add_stats(anova_lme4) %>%
  add_stats(anova_models)

# Inspect output
anova_lme4
anova_models

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/testthat/data/lme4.json")

# Cleanup -----------------------------------------------------------------

rm(anova_lme4, anova_models, lme4, lme4_ML, lme4_slopes, df, statistics)
