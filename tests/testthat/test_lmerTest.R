# lmer() ------------------------------------------------------------------

test_that("lmerTest fixed effects are correctly extracted", {
  skip_if_not_installed("lmerTest")
  model <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  result <- tidy_stats(model)

  expect_equal(result$method, "Linear mixed model")
  expect_equal(result$REML_criterion_at_convergence, 1743.628, tolerance = 1e-3)

  # Fixed effects: groups[[3]] = Fixed effects, groups[[1]] = Coefficients
  intercept <- result$groups[[3]]$groups[[1]]$groups[[1]]$statistics
  days <- result$groups[[3]]$groups[[1]]$groups[[2]]$statistics

  expect_equal(intercept[[1]]$value, 251.4051,    tolerance = 1e-4) # estimate
  expect_equal(intercept[[2]]$value, 6.824597,    tolerance = 1e-4) # SE
  expect_equal(intercept[[4]]$value, 36.83809,    tolerance = 1e-4) # t
  expect_equal(intercept[[5]]$value, 1.171558e-17, tolerance = 1e-4) # p

  expect_equal(days[[1]]$value, 10.46729,         tolerance = 1e-4) # estimate
  expect_equal(days[[2]]$value, 1.54579,           tolerance = 1e-4) # SE
  expect_equal(days[[4]]$value, 6.771481,          tolerance = 1e-4) # t
  expect_equal(days[[5]]$value, 3.263824e-06,      tolerance = 1e-4) # p
})

test_that("lmerTest 2 fixed effects are correctly extracted", {
  skip_if_not_installed("lmerTest")
  model <- lmerTest::lmer(
    Informed.liking ~ Gender +
      Information * Product +
      (1 | Consumer) +
      (1 | Consumer:Product),
    data = lmerTest::ham
  )
  result <- tidy_stats(model)

  expect_equal(result$REML_criterion_at_convergence, 2705.504, tolerance = 1e-3)

  fe_groups <- result$groups[[3]]$groups[[1]]$groups
  expect_equal(fe_groups[[1]]$statistics[[1]]$value, 5.849029, tolerance = 1e-4) # intercept estimate
  expect_equal(
    fe_groups[[2]]$statistics[[5]]$value,
    0.3513501,
    tolerance = 1e-4
  ) # Gender2 p
  expect_equal(
    fe_groups[[4]]$statistics[[5]]$value,
    0.01714885,
    tolerance = 1e-4
  ) # Product2 p
  expect_equal(
    fe_groups[[7]]$statistics[[5]]$value,
    0.3901831,
    tolerance = 1e-4
  ) # Information2:Product2 p
})

test_that("lmerTest ML fixed effects are correctly extracted", {
  skip_if_not_installed("lmerTest")
  model <- lmerTest::lmer(
    Reaction ~ Days + (Days | Subject),
    lme4::sleepstudy,
    REML = FALSE
  )
  result <- tidy_stats(model)

  fe_groups <- result$groups[[3]]$groups[[1]]$groups
  expect_equal(fe_groups[[1]]$statistics[[1]]$value, 251.4051, tolerance = 1e-4) # intercept estimate
  expect_equal(
    fe_groups[[1]]$statistics[[5]]$value,
    1.263752e-18,
    tolerance = 1e-4
  ) # intercept p
  expect_equal(fe_groups[[2]]$statistics[[1]]$value, 10.46729, tolerance = 1e-4) # Days estimate
  expect_equal(
    fe_groups[[2]]$statistics[[5]]$value,
    1.652228e-06,
    tolerance = 1e-4
  ) # Days p
})

# anova.merMod() ----------------------------------------------------------

test_that("lmerTest anova (Satterthwaite) extracts correct statistics", {
  skip_if_not_installed("lmerTest")
  m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  result <- tidy_stats(anova(m))

  days <- result$groups[[1]]$groups[[1]]$statistics
  expect_equal(days[[1]]$value, 30030.94,    tolerance = 1e-2) # SS
  expect_equal(days[[2]]$value, 30030.94,    tolerance = 1e-2) # MS
  expect_equal(days[[3]]$value, 45.85296,    tolerance = 1e-4) # F
  expect_equal(days[[4]]$value, 1,           tolerance = 1e-6) # df numerator
  expect_equal(days[[5]]$value, 16.99998,    tolerance = 1e-4) # df denominator
  expect_equal(days[[6]]$value, 3.263824e-6, tolerance = 1e-4) # p
})

test_that("lmerTest anova (lme4 ddf) extracts correct statistics", {
  skip_if_not_installed("lmerTest")
  m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  result <- tidy_stats(anova(m, ddf = "lme4"))

  expect_equal(result$method, "ANOVA")
  days <- result$groups[[1]]$groups[[1]]$statistics
  expect_equal(days[[2]]$value, 30030.94, tolerance = 1e-2) # SS
})

test_that("lmerTest anova fit comparison extracts correct statistics", {
  skip_if_not_installed("lmerTest")
  m0 <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  result <- tidy_stats(suppressMessages(anova(m0, m)))

  m_stats <- result$groups[[1]]$groups[[2]]$statistics
  expect_equal(m_stats[[1]]$value, 6,           tolerance = 1e-6) # n parameters
  expect_equal(m_stats[[2]]$value, 1763.939,    tolerance = 1e-3) # AIC
  expect_equal(m_stats[[5]]$value, 1751.939,    tolerance = 1e-3) # -2*log(L)
  expect_equal(m_stats[[6]]$value, 42.1393,     tolerance = 1e-3) # chi-sq
  expect_equal(m_stats[[7]]$value, 2,           tolerance = 1e-6) # df
  expect_equal(m_stats[[8]]$value, 7.072413e-10, tolerance = 1e-4) # p
})
