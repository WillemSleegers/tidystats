
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lmerTest)

# Create an empty list
statistics <- list()

# lmer() ------------------------------------------------------------------

# Run analyses
lmerTest1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
lmerTest2 <- lmer(
  Informed.liking ~ 
    Gender + Information * Product + (1 | Consumer) + (1 | Consumer:Product), 
  data = ham
)
lmerTest_ML <- lmer(
  Reaction ~ Days + (Days | Subject), sleepstudy, 
  REML = FALSE
)

# Add stats
statistics <- statistics %>%
  add_stats(lmerTest1) %>%
  add_stats(lmerTest2) %>%
  add_stats(lmerTest_ML)

# Inspect output
summary(lmerTest1)
summary(lmerTest2)
summary(lmerTest_ML)

# anova() -----------------------------------------------------------------

# Run analyses
m0 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
m <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

anova_lmerTest <- anova(m)
anova_lmerTest_lme4 <- anova(m, ddf = "lme4")
anova_lmerTest_fit <- anova(m0, m)

# Add stats
statistics <- statistics %>%
  add_stats(anova_lmerTest) %>%
  add_stats(anova_lmerTest_lme4) %>%
  add_stats(anova_lmerTest_fit)

# Inspect output
anova_lmerTest
anova_lmerTest_lme4
anova_lmerTest_fit

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/testthat/data/lmerTest.json")

# Cleanup -----------------------------------------------------------------

rm(
  anova_lmerTest, anova_lmerTest_fit, anova_lmerTest_lme4, lmerTest_ML, 
  lmerTest1, lmerTest2, m, m0, df, statistics
)
