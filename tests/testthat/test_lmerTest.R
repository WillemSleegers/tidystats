# Setup -------------------------------------------------------------------

expected_statistics <- read_stats("../data/lmerTest.json")

# lmer() ------------------------------------------------------------------

test_that("lmerTest 1 works", {
  model <- lmerTest::lmer(
    Reaction ~ Days + (Days | Subject), lme4::sleepstudy
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$lmerTest1
  )
})

test_that("lmerTest 2 works", {
  model <- lmerTest::lmer(Informed.liking ~ Gender + Information *
    Product + (1 | Consumer) + (1 | Consumer:Product), data = lmerTest::ham)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$lmerTest2
  )
})

test_that("lmerTest ML works", {
  model <- lmerTest::lmer(
    Reaction ~ Days + (Days | Subject), sleepstudy,
    REML = FALSE
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$lmerTest_ML
  )
})

# anova.merMod() ----------------------------------------------------------

test_that("lmerTest anova works", {
  m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  model <- anova(m)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$anova_lmerTest
  )
})

test_that("lmerTest anova lme4 works", {
  m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  model <- anova(m, ddf = "lme4")

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$anova_lmerTest_lme4
  )
})

test_that("lmerTest anova fit works", {
  m0 <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
  m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  model <- anova(m0, m)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$anova_lmerTest_fit
  )
})
