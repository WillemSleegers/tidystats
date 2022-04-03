
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(lme4)
library(lmerTest)

# Create an empty list
results <- list()

# lme4::lmer() ------------------------------------------------------------

# Run tests
lme4 <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
lme4_ML <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy, 
  REML = FALSE)
lme4_slopes <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy, 
  REML = FALSE)

summary(lme4)
summary(lme4_ML)
summary(lme4_slopes)

# Tidy results
temp <- tidy_stats(lme4)
temp <- tidy_stats(lme4_ML)
temp <- tidy_stats(lme4_slopes)

# Add stats
results <- results %>%
  add_stats(lme4) %>%
  add_stats(lme4_ML) %>%
  add_stats(lme4_slopes) 

# anova.merMod ------------------------------------------------------------

# Run tests
anova_lme4 <- anova(lme4)
anova_models <- anova(lme4, lme4_slopes)

# Tidy stats
temp <- tidy_stats(anova_lme4)
temp <- tidy_stats(anova_models)

# Add stats
results <- results %>%
  add_stats(anova_lme4) %>%
  add_stats(anova_models)

# Save stats
write_stats(results, "inst/test_data/lmer.json")

# lmerTest’s lmer() -------------------------------------------------------

results <- list()

# Load packages
library(lme4)
library(lmerTest)

# Run multilevel models
lmerTest1 <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
lmerTest2 <- lmerTest::lmer(Informed.liking ~ Gender + Information * 
    Product + (1 | Consumer) + (1 | Consumer:Product), data = lmerTest::ham)

lmerTest_ML <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), sleepstudy, 
  REML = FALSE)

summary(lmerTest1)
summary(lmerTest2)
summary(lmerTest_ML)

# Tidy results
temp <- tidy_stats(lmerTest1)
temp <- tidy_stats(lmerTest2)
temp <- tidy_stats(lmerTest_ML)

# Add stats
results <- results %>%
  add_stats(lmerTest1) %>%
  add_stats(lmerTest2)

# anova.lmerModLmerTest ---------------------------------------------------

m0 <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

anova_lmerTest <- anova(m)
anova_lmerTest_lme4 <- anova(m, ddf = "lme4")
anova_lmerTest_fit <- anova(m0, m)

anova_lmerTest
anova_lmerTest_lme4
anova_lmerTest_fit

# Tidy stats
temp <- tidy_stats(anova_lmerTest)
temp <- tidy_stats(anova_lmerTest_lme4)
temp <- tidy_stats(anova_lmerTest_fit)

# Add stats
results <- results %>%
  add_stats(anova_lmerTest) %>%
  add_stats(anova_lmerTest_lme4) %>%
  add_stats(anova_lmerTest_fit)

write_stats(results, "inst/test_data/lmerTest.json")
