
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(lme4)
library(lmerTest)

# Create an empty list
results <- list()

# lme4::lmer() ------------------------------------------------------------

# Run analyses
lme4 <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
lme4_ML <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy, 
  REML = FALSE)
lme4_slopes <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy, 
  REML = FALSE)

# Add stats
results <- results %>%
  add_stats(lme4) %>%
  add_stats(lme4_ML) %>%
  add_stats(lme4_slopes) 

summary(lme4)
summary(lme4_ML)
summary(lme4_slopes)

# anova(): anova.merMod ---------------------------------------------------

# Run analyses
anova_lme4 <- anova(lme4)
anova_models <- anova(lme4, lme4_slopes)

# Add stats
results <- results %>%
  add_stats(anova_lme4) %>%
  add_stats(anova_models)

anova_lme4
anova_models

# lmerTest::lmer() -------------------------------------------------------

# Run analyses
lmerTest1 <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
lmerTest2 <- lmerTest::lmer(Informed.liking ~ Gender + Information * 
    Product + (1 | Consumer) + (1 | Consumer:Product), data = lmerTest::ham)
lmerTest_ML <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), sleepstudy, 
  REML = FALSE)

# Add stats
results <- results %>%
  add_stats(lmerTest1) %>%
  add_stats(lmerTest2) %>%
  add_stats(lmerTest_ML)

summary(lmerTest1)
summary(lmerTest2)
summary(lmerTest_ML)

# anova(): anova.lmerModLmerTest ------------------------------------------

# Run analyses
m0 <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

anova_lmerTest <- anova(m)
anova_lmerTest_lme4 <- anova(m, ddf = "lme4")
anova_lmerTest_fit <- anova(m0, m)

# Add stats
results <- results %>%
  add_stats(anova_lmerTest) %>%
  add_stats(anova_lmerTest_lme4) %>%
  add_stats(anova_lmerTest_fit)

anova_lmerTest
anova_lmerTest_lme4
anova_lmerTest_fit

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/lmer.json")
