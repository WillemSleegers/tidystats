# Setup -------------------------------------------------------------------

path <- system.file("tests/data/lmer.json", package = "tidystats")
expected_statistics <- read_stats(path)

# lmer() ------------------------------------------------------------------

test_that("lme4 works", {
  model <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$lme4
  )
})

test_that("lme4 ML works", {
  model <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy,
    REML = FALSE
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$lme4_ML
  )
})

test_that("lme4 slopes works", {
  model <- lme4::lmer(Reaction ~ Days + (Days || Subject), lme4::sleepstudy)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$lme4_slopes
  )
})

# anova.merMod() ----------------------------------------------------------

test_that("lme4 anova works", {
  lme4 <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  model <- anova(lme4)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$anova_lme4
  )
})

test_that("lme4 anova model comparison works", {
  lme4 <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  lme4_slopes <- lme4::lmer(
    Reaction ~ Days + (Days || Subject),
    lme4::sleepstudy
  )
  model <- anova(lme4, lme4_slopes)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$anova_models
  )
})
