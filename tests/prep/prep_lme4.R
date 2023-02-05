# Setup -------------------------------------------------------------------

statistics <- list()

# lmer() ------------------------------------------------------------------

lme4 <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
lme4_ML <- lme4::lmer(
  Reaction ~ Days + (1 | Subject), lme4::sleepstudy,
  REML = FALSE
)
lme4_slopes <- lme4::lmer(Reaction ~ Days + (Days || Subject), lme4::sleepstudy)

statistics <- statistics |>
  add_stats(lme4) |>
  add_stats(lme4_ML) |>
  add_stats(lme4_slopes)

summary(lme4)
summary(lme4_ML)
summary(lme4_slopes)

# anova() -----------------------------------------------------------------

anova_lme4 <- anova(lme4)
anova_models <- anova(lme4, lme4_slopes)

statistics <- statistics |>
  add_stats(anova_lme4) |>
  add_stats(anova_models)

anova_lme4
anova_models

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/lme4.json")

# Cleanup -----------------------------------------------------------------

rm(anova_lme4, anova_models, lme4, lme4_ML, lme4_slopes, df, statistics)
