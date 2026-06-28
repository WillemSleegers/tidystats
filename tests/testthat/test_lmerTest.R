# Compare against the model object's own coefficient table / anova table rather
# than hard-coded constants, so the tests verify tidy_stats's extraction
# independent of the lmerTest/lme4 versions (which compute the Satterthwaite
# degrees of freedom and p-values, and have renamed columns in the past).

# lmer() ------------------------------------------------------------------

test_that("lmerTest fixed effects are correctly extracted", {
  skip_if_not_installed("lmerTest")
  model <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  result <- tidy_stats(model)
  coefs <- coef(summary(model))

  expect_equal(result$method, "Linear mixed model")
  expect_equal(
    result$REML_criterion_at_convergence,
    summary(model)$AICtab[["REML"]]
  )

  fe_groups <- result$groups[[3]]$groups[[1]]$groups
  for (i in seq_len(nrow(coefs))) {
    expect_equal(fe_groups[[i]]$name, rownames(coefs)[i])
    expect_equal(fe_groups[[i]]$statistics[[1]]$value, coefs[i, "Estimate"])   # estimate
    expect_equal(fe_groups[[i]]$statistics[[2]]$value, coefs[i, "Std. Error"]) # SE
    expect_equal(fe_groups[[i]]$statistics[[4]]$value, coefs[i, "t value"])    # t
    expect_equal(fe_groups[[i]]$statistics[[5]]$value, coefs[i, "Pr(>|t|)"])   # p
  }
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
  coefs <- coef(summary(model))

  fe_groups <- result$groups[[3]]$groups[[1]]$groups
  for (i in seq_len(nrow(coefs))) {
    expect_equal(fe_groups[[i]]$name, rownames(coefs)[i])
    expect_equal(fe_groups[[i]]$statistics[[1]]$value, coefs[i, "Estimate"]) # estimate
    expect_equal(fe_groups[[i]]$statistics[[5]]$value, coefs[i, "Pr(>|t|)"]) # p
  }
})

test_that("lmerTest ML fixed effects are correctly extracted", {
  skip_if_not_installed("lmerTest")
  model <- lmerTest::lmer(
    Reaction ~ Days + (Days | Subject),
    lme4::sleepstudy,
    REML = FALSE
  )
  result <- tidy_stats(model)
  coefs <- coef(summary(model))

  fe_groups <- result$groups[[3]]$groups[[1]]$groups
  for (i in seq_len(nrow(coefs))) {
    expect_equal(fe_groups[[i]]$statistics[[1]]$value, coefs[i, "Estimate"]) # estimate
    expect_equal(fe_groups[[i]]$statistics[[5]]$value, coefs[i, "Pr(>|t|)"]) # p
  }
})

# anova.merMod() ----------------------------------------------------------

test_that("lmerTest anova (Satterthwaite) extracts correct statistics", {
  skip_if_not_installed("lmerTest")
  m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  at <- anova(m)
  result <- tidy_stats(at)

  days <- result$groups[[1]]$groups[[1]]$statistics
  expect_equal(days[[1]]$value, at[["Sum Sq"]][1])  # SS
  expect_equal(days[[2]]$value, at[["Mean Sq"]][1]) # MS
  expect_equal(days[[3]]$value, at[["F value"]][1]) # F
  expect_equal(days[[4]]$value, at[["NumDF"]][1])   # df numerator
  expect_equal(days[[5]]$value, at[["DenDF"]][1])   # df denominator
  expect_equal(days[[6]]$value, at[["Pr(>F)"]][1])  # p
})

test_that("lmerTest anova (lme4 ddf) extracts correct statistics", {
  skip_if_not_installed("lmerTest")
  m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  at <- anova(m, ddf = "lme4")
  result <- tidy_stats(at)

  expect_equal(result$method, "ANOVA")
  days <- result$groups[[1]]$groups[[1]]$statistics
  expect_equal(days[[2]]$value, at[["Sum Sq"]][1]) # SS
})

test_that("lmerTest anova fit comparison extracts correct statistics", {
  skip_if_not_installed("lmerTest")
  m0 <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  m <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  at <- suppressMessages(anova(m0, m))
  result <- tidy_stats(at)

  # lme4 renamed the "deviance" column to "-2*log(L)"; read whichever exists.
  dev <- if ("-2*log(L)" %in% names(at)) {
    at[["-2*log(L)"]]
  } else {
    at[["deviance"]]
  }

  m_stats <- result$groups[[1]]$groups[[2]]$statistics
  expect_equal(m_stats[[1]]$value, at$npar[2])           # n parameters
  expect_equal(m_stats[[2]]$value, at$AIC[2])            # AIC
  expect_equal(m_stats[[5]]$value, dev[2])               # -2*log(L)
  expect_equal(m_stats[[6]]$value, at$Chisq[2])          # chi-sq
  expect_equal(m_stats[[7]]$value, at$Df[2])             # df
  expect_equal(m_stats[[8]]$value, at[["Pr(>Chisq)"]][2]) # p
})
