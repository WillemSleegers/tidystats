# Setup -------------------------------------------------------------------

expected_statistics <- read_stats("../data/lme4.json")

# lmer() ------------------------------------------------------------------

test_that("lme4 works", {
  skip_if(packageVersion("lme4") < "1.1.37")
  model <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$lme4
  )
})

test_that("lme4 ML works", {
  skip_if(packageVersion("lme4") < "1.1.37")
  model <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy,
    REML = FALSE
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$lme4_ML
  )
})

test_that("lme4 slopes works", {
  skip_if(packageVersion("lme4") < "1.1.37")
  model <- lme4::lmer(Reaction ~ Days + (Days || Subject), lme4::sleepstudy)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$lme4_slopes
  )
})

# anova.merMod() ----------------------------------------------------------

test_that("lme4 anova works", {
  skip_if(packageVersion("lme4") < "1.1.37")
  lme4 <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  model <- anova(lme4)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$anova_lme4
  )
})

test_that("lme4 anova model comparison works", {
  skip_if(packageVersion("lme4") < "1.1.37")
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
