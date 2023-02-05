# Setup -------------------------------------------------------------------

statistics <- list()

# lmer() ------------------------------------------------------------------

lmerTest1 <- lmerTest::lmer(
  Reaction ~ Days + (Days | Subject), lme4::sleepstudy
)
lmerTest2 <- lmerTest::lmer(
  Informed.liking ~
    Gender + Information * Product + (1 | Consumer) + (1 | Consumer:Product),
  data = lmerTest::ham
)
lmerTest_ML <- lmerTest::lmer(
  Reaction ~ Days + (Days | Subject), lme4::sleepstudy,
  REML = FALSE
)

statistics <- statistics |>
  add_stats(lmerTest1) |>
  add_stats(lmerTest2) |>
  add_stats(lmerTest_ML)

summary(lmerTest1)
summary(lmerTest2)
summary(lmerTest_ML)

# anova() -----------------------------------------------------------------

m0 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
m <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

anova_lmerTest <- anova(m)
anova_lmerTest_lme4 <- anova(m, ddf = "lme4")
anova_lmerTest_fit <- anova(m0, m)

statistics <- statistics |>
  add_stats(anova_lmerTest) |>
  add_stats(anova_lmerTest_lme4) |>
  add_stats(anova_lmerTest_fit)

anova_lmerTest
anova_lmerTest_lme4
anova_lmerTest_fit

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/lmerTest.json")

# Cleanup -----------------------------------------------------------------

rm(
  anova_lmerTest, anova_lmerTest_fit, anova_lmerTest_lme4, lmerTest_ML,
  lmerTest1, lmerTest2, m, m0, df, statistics
)
