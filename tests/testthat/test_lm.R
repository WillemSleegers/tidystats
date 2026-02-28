# lm() --------------------------------------------------------------------

test_that("lm works", {
  ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
  trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
  group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
  weight <- c(ctl, trt)

  result <- tidy_stats(lm(weight ~ group))

  expect_equal(result$method, "Linear regression")

  # Model fit statistics (groups[[1]])
  model_stats <- result$groups[[1]]$statistics
  expect_equal(model_stats[[1]]$value, 0.0730776,  tolerance = 1e-4) # R squared
  expect_equal(model_stats[[3]]$value, 1.419101,   tolerance = 1e-4) # F
  expect_equal(model_stats[[6]]$value, 0.2490232,  tolerance = 1e-4) # p

  # Coefficients (groups[[2]])
  coefs <- result$groups[[2]]$groups
  expect_equal(coefs[[1]]$statistics[[1]]$value, 5.032,         tolerance = 1e-4) # intercept estimate
  expect_equal(coefs[[1]]$statistics[[5]]$value, 9.547128e-15,  tolerance = 1e-4) # intercept p
  expect_equal(coefs[[2]]$statistics[[1]]$value, -0.371,        tolerance = 1e-4) # groupTrt estimate
  expect_equal(coefs[[2]]$statistics[[5]]$value, 0.2490232,     tolerance = 1e-4) # groupTrt p
})

test_that("lm without an intercept works", {
  ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
  trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
  group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
  weight <- c(ctl, trt)

  result <- tidy_stats(lm(weight ~ group - 1))

  coefs <- result$groups[[2]]$groups
  expect_equal(coefs[[1]]$name, "groupCtl")
  expect_equal(coefs[[1]]$statistics[[1]]$value, 5.032,        tolerance = 1e-4) # estimate
  expect_equal(coefs[[1]]$statistics[[5]]$value, 9.547128e-15, tolerance = 1e-4) # p
  expect_equal(coefs[[2]]$name, "groupTrt")
  expect_equal(coefs[[2]]$statistics[[1]]$value, 4.661,        tolerance = 1e-4) # estimate
  expect_equal(coefs[[2]]$statistics[[5]]$value, 3.615345e-14, tolerance = 1e-4) # p
})

test_that("lm anova works", {
  fit <- lm(sr ~ ., data = LifeCycleSavings)

  result <- tidy_stats(anova(fit))

  terms <- result$groups[[1]]$groups
  expect_equal(terms[[1]]$name, "pop15")
  expect_equal(terms[[1]]$statistics[[1]]$value, 204.1176,      tolerance = 1e-4) # SS
  expect_equal(terms[[1]]$statistics[[3]]$value, 14.11573,      tolerance = 1e-4) # F
  expect_equal(terms[[1]]$statistics[[6]]$value, 0.0004921955,  tolerance = 1e-4) # p
  expect_equal(terms[[4]]$name, "ddpi")
  expect_equal(terms[[4]]$statistics[[3]]$value, 4.360496,      tolerance = 1e-4) # F
  expect_equal(terms[[4]]$statistics[[6]]$value, 0.04247114,    tolerance = 1e-4) # p
})

test_that("lm model comparison anova works", {
  fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
  fit1 <- update(fit0, . ~ . + pop15)
  fit2 <- update(fit1, . ~ . + pop75)
  fit3 <- update(fit2, . ~ . + dpi)
  fit4 <- update(fit3, . ~ . + ddpi)

  result <- tidy_stats(anova(fit0, fit1, fit2, fit3, fit4, test = "F"))

  models <- result$groups[[1]]$groups
  expect_equal(models[[1]]$statistics[[1]]$value, 983.6282,     tolerance = 1e-3) # RSS
  expect_equal(models[[2]]$statistics[[3]]$value, 14.11573,     tolerance = 1e-4) # F
  expect_equal(models[[2]]$statistics[[6]]$value, 0.0004921955, tolerance = 1e-4) # p
  expect_equal(models[[5]]$statistics[[3]]$value, 4.360496,     tolerance = 1e-4) # F
  expect_equal(models[[5]]$statistics[[6]]$value, 0.04247114,   tolerance = 1e-4) # p
})

test_that("lm model comparison anova in another order works", {
  fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
  fit1 <- update(fit0, . ~ . + pop15)
  fit2 <- update(fit1, . ~ . + pop75)
  fit3 <- update(fit2, . ~ . + dpi)
  fit4 <- update(fit3, . ~ . + ddpi)

  result <- tidy_stats(anova(fit4, fit2, fit0, test = "F"))

  models <- result$groups[[1]]$groups
  expect_equal(models[[1]]$statistics[[1]]$value, 650.713,      tolerance = 1e-3) # RSS
  expect_equal(models[[2]]$statistics[[3]]$value, 2.609041,     tolerance = 1e-4) # F
  expect_equal(models[[2]]$statistics[[6]]$value, 0.08470885,   tolerance = 1e-4) # p
  expect_equal(models[[3]]$statistics[[3]]$value, 8.902321,     tolerance = 1e-4) # F
  expect_equal(models[[3]]$statistics[[6]]$value, 0.0005526717, tolerance = 1e-4) # p
})

test_that("lm model comparison anova chi-squared works", {
  fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
  fit1 <- update(fit0, . ~ . + pop15)
  fit2 <- update(fit1, . ~ . + pop75)
  fit3 <- update(fit2, . ~ . + dpi)
  fit4 <- update(fit3, . ~ . + ddpi)

  result <- tidy_stats(anova(fit4, fit2, fit0, test = "Chisq"))

  models <- result$groups[[1]]$groups
  # subgroup 2: chi-sq test (no F statistic)
  sg2 <- models[[2]]$statistics
  expect_equal(sg2[[1]]$value, 726.168,    tolerance = 1e-3) # RSS
  expect_equal(sg2[[5]]$value, 0.07360509, tolerance = 1e-4) # p
})

test_that("lm model comparison anova Cp works", {
  fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
  fit1 <- update(fit0, . ~ . + pop15)
  fit2 <- update(fit1, . ~ . + pop75)
  fit3 <- update(fit2, . ~ . + dpi)
  fit4 <- update(fit3, . ~ . + ddpi)

  result <- tidy_stats(anova(fit4, fit2, fit0, test = "Cp"))

  models <- result$groups[[1]]$groups
  sg2 <- models[[2]]$statistics
  expect_equal(sg2[[1]]$value, 726.168,  tolerance = 1e-3) # RSS
  expect_equal(sg2[[5]]$value, 812.9297, tolerance = 1e-4) # Cp
})
