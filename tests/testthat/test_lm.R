# Setup -------------------------------------------------------------------

expected_statistics <- read_stats("../data/lm.json")

# lm() --------------------------------------------------------------------

test_that("lm works", {
  ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
  trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
  group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
  weight <- c(ctl, trt)

  model <- lm(weight ~ group)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$lm
  )
})

test_that("lm without an intercept works", {
  ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
  trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
  group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
  weight <- c(ctl, trt)

  model <- lm(weight ~ group - 1)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$lm_wo_intercept
  )
})

test_that("lm anova works", {
  fit <- lm(sr ~ ., data = LifeCycleSavings)

  model <- anova(fit)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$anova_lm
  )
})

test_that("lm model comparison anova works", {
  fit <- lm(sr ~ ., data = LifeCycleSavings)
  fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
  fit1 <- update(fit0, . ~ . + pop15)
  fit2 <- update(fit1, . ~ . + pop75)
  fit3 <- update(fit2, . ~ . + dpi)
  fit4 <- update(fit3, . ~ . + ddpi)

  model <- anova(fit0, fit1, fit2, fit3, fit4, test = "F")

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$anova_lm_fits
  )
})

test_that("lm model comparison anova in another order works", {
  fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
  fit1 <- update(fit0, . ~ . + pop15)
  fit2 <- update(fit1, . ~ . + pop75)
  fit3 <- update(fit2, . ~ . + dpi)
  fit4 <- update(fit3, . ~ . + ddpi)

  model <- anova(fit4, fit2, fit0, test = "F")

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$anova_lm_order
  )
})

test_that("lm model comparison anova chi-squared works", {
  fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
  fit1 <- update(fit0, . ~ . + pop15)
  fit2 <- update(fit1, . ~ . + pop75)
  fit3 <- update(fit2, . ~ . + dpi)
  fit4 <- update(fit3, . ~ . + ddpi)

  model <- anova(fit4, fit2, fit0, test = "Chisq")

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$anova_lm_chisq
  )
})

test_that("lm model comparison anova Cp works", {
  fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
  fit1 <- update(fit0, . ~ . + pop15)
  fit2 <- update(fit1, . ~ . + pop75)
  fit3 <- update(fit2, . ~ . + dpi)
  fit4 <- update(fit3, . ~ . + ddpi)

  model <- anova(fit4, fit2, fit0, test = "Cp")

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$anova_lm_cp
  )
})
