# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
results <- list()

# anova: anova.lm ---------------------------------------------------------

# Run tests
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

anova_lm
anova_lm_fits
anova_lm_order
anova_lm_chisq
anova_lm_cp


# Tidy stats
temp <- tidy_stats(anova_lm)
temp <- tidy_stats(anova_lm_fits)
temp <- tidy_stats(anova_lm_order)
temp <- tidy_stats(anova_lm_chisq)
temp <- tidy_stats(anova_lm_cp)

# Add stats
results <- results %>%
  add_stats(anova_lm) %>%
  add_stats(anova_lm_fits) %>%
  add_stats(anova_lm_order) %>%
  add_stats(anova_lm_chisq) %>%
  add_stats(anova_lm_cp)

# anova: anova.glm --------------------------------------------------------

# Get data
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)
d.AD <- data.frame(treatment, outcome, counts)

# Run tests
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
glm.D93a <- update(glm.D93, ~treatment * outcome)

anova_glm <- anova(glm.D93)
anova_glm_cp <- anova(glm.D93, test = "Cp")
anova_glm_chisq <- anova(glm.D93, test = "Chisq")
anova_glm_rao <- anova(glm.D93, glm.D93a, test = "Rao")

anova_glm
anova_glm_cp
anova_glm_chisq
anova_glm_rao


# Tidy stats
temp <- tidy_stats(anova_glm)
temp <- tidy_stats(anova_glm_cp)
temp <- tidy_stats(anova_glm_chisq)
temp <- tidy_stats(anova_glm_rao)

# Add stats
results <- results %>%
  add_stats(anova_glm) %>%
  add_stats(anova_glm_cp) %>%
  add_stats(anova_glm_chisq) %>%
  add_stats(anova_glm_rao)

# lme4 --------------------------------------------------------------------

# Run tests
lme4 <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
lme4_ML <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy, 
  REML = FALSE)
lme4_slopes <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy, 
  REML = FALSE)

# Run tests
anova_lme4 <- anova(lme4)
anova_models <- anova(lme4, lme4_slopes)

anova_lme4
anova_models

# Tidy stats
temp <- tidy_stats(anova_lme4)
temp <- tidy_stats(anova_models)

# Add stats
results <- results %>%
  add_stats(anova_lme4) %>%
  add_stats(anova_models)

# anova: write_stats() ----------------------------------------------------

write_stats(results, "inst/test_data/anova.json")
