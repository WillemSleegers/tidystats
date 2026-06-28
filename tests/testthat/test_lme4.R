# Compare against the model object's own coefficient table / anova table rather
# than hard-coded constants, so the tests verify tidy_stats's extraction
# independent of the lme4 version (which has, e.g., renamed columns and changed
# how some statistics are computed in the past).

# lmer() ------------------------------------------------------------------

test_that("lme4 works", {
  skip_if_not_installed("lme4")

  model <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  result <- tidy_stats(model)
  coefs <- coef(summary(model))

  expect_equal(result$method, "Linear mixed model")
  expect_equal(
    result$REML_criterion_at_convergence,
    summary(model)$AICtab[["REML"]]
  )

  fe <- result$groups[[3]]$groups[[1]]$groups
  for (i in seq_len(nrow(coefs))) {
    expect_equal(fe[[i]]$name, rownames(coefs)[i])
    expect_equal(fe[[i]]$statistics[[1]]$value, coefs[i, "Estimate"])   # estimate
    expect_equal(fe[[i]]$statistics[[2]]$value, coefs[i, "Std. Error"]) # SE
    expect_equal(fe[[i]]$statistics[[3]]$value, coefs[i, "t value"])    # t
  }
})

test_that("lme4 ML works", {
  skip_if_not_installed("lme4")

  model <- lme4::lmer(
    Reaction ~ Days + (1 | Subject),
    lme4::sleepstudy,
    REML = FALSE
  )
  result <- tidy_stats(model)
  coefs <- coef(summary(model))

  fe <- result$groups[[3]]$groups[[1]]$groups
  for (i in seq_len(nrow(coefs))) {
    expect_equal(fe[[i]]$statistics[[1]]$value, coefs[i, "Estimate"]) # estimate
  }
})

test_that("lme4 slopes works", {
  skip_if_not_installed("lme4")

  model <- lme4::lmer(Reaction ~ Days + (Days || Subject), lme4::sleepstudy)
  result <- tidy_stats(model)
  coefs <- coef(summary(model))

  expect_equal(result$method, "Linear mixed model")
  fe <- result$groups[[3]]$groups[[1]]$groups
  for (i in seq_len(nrow(coefs))) {
    expect_equal(fe[[i]]$statistics[[1]]$value, coefs[i, "Estimate"]) # estimate
  }
})

# anova.merMod() ----------------------------------------------------------

test_that("lme4 anova works", {
  skip_if_not_installed("lme4")

  lme4_m <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  at <- anova(lme4_m)
  result <- tidy_stats(at)

  terms <- result$groups[[1]]$groups
  expect_equal(terms[[1]]$name, "Days")
  expect_equal(terms[[1]]$statistics[[2]]$value, at["Days", "Sum Sq"])  # SS
  expect_equal(terms[[1]]$statistics[[4]]$value, at["Days", "F value"]) # F
})

test_that("lme4 anova model comparison works", {
  skip_if_not_installed("lme4")

  lme4_m <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  lme4_slopes <- lme4::lmer(
    Reaction ~ Days + (Days || Subject),
    lme4::sleepstudy
  )
  at <- suppressMessages(anova(lme4_m, lme4_slopes))
  result <- tidy_stats(at)

  models <- result$groups[[1]]$groups
  expect_equal(models[[2]]$statistics[[6]]$value, at$Chisq[2])           # chi-sq
  expect_equal(models[[2]]$statistics[[7]]$value, at$Df[2])              # df
  expect_equal(models[[2]]$statistics[[8]]$value, at[["Pr(>Chisq)"]][2]) # p
})
