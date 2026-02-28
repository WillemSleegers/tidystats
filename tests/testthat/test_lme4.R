# lmer() ------------------------------------------------------------------

test_that("lme4 works", {
  skip_if_not_installed("lme4")

  result <- tidy_stats(lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy))

  expect_equal(result$method, "Linear mixed model")
  expect_equal(result$REML_criterion_at_convergence, 1786.465, tolerance = 1e-3)

  fe <- result$groups[[3]]$groups[[1]]$groups
  expect_equal(fe[[1]]$statistics[[1]]$value, 251.4051,  tolerance = 1e-4) # intercept estimate
  expect_equal(fe[[1]]$statistics[[3]]$value, 25.79383,  tolerance = 1e-4) # intercept t
  expect_equal(fe[[2]]$statistics[[1]]$value, 10.46729,  tolerance = 1e-4) # Days estimate
  expect_equal(fe[[2]]$statistics[[3]]$value, 13.01543,  tolerance = 1e-4) # Days t
})

test_that("lme4 ML works", {
  skip_if_not_installed("lme4")

  result <- tidy_stats(lme4::lmer(
    Reaction ~ Days + (1 | Subject),
    lme4::sleepstudy,
    REML = FALSE
  ))

  fe <- result$groups[[3]]$groups[[1]]$groups
  expect_equal(fe[[1]]$statistics[[1]]$value, 251.4051, tolerance = 1e-4) # intercept estimate
  expect_equal(fe[[2]]$statistics[[1]]$value, 10.46729, tolerance = 1e-4) # Days estimate
})

test_that("lme4 slopes works", {
  skip_if_not_installed("lme4")

  result <- tidy_stats(lme4::lmer(Reaction ~ Days + (Days || Subject), lme4::sleepstudy))

  expect_equal(result$method, "Linear mixed model")
  fe <- result$groups[[3]]$groups[[1]]$groups
  expect_equal(fe[[1]]$statistics[[1]]$value, 251.4051, tolerance = 1e-4) # intercept
  expect_equal(fe[[2]]$statistics[[1]]$value, 10.46729, tolerance = 1e-4) # Days
})

# anova.merMod() ----------------------------------------------------------

test_that("lme4 anova works", {
  skip_if_not_installed("lme4")

  lme4_m <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  result <- tidy_stats(anova(lme4_m))

  terms <- result$groups[[1]]$groups
  expect_equal(terms[[1]]$name, "Days")
  expect_equal(terms[[1]]$statistics[[2]]$value, 162702.7, tolerance = 1e-1) # SS
  expect_equal(terms[[1]]$statistics[[4]]$value, 169.4014, tolerance = 1e-3) # F
})

test_that("lme4 anova model comparison works", {
  skip_if_not_installed("lme4")

  lme4_m <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  lme4_slopes <- lme4::lmer(
    Reaction ~ Days + (Days || Subject),
    lme4::sleepstudy
  )
  result <- tidy_stats(suppressMessages(anova(lme4_m, lme4_slopes)))

  models <- result$groups[[1]]$groups
  expect_equal(models[[2]]$statistics[[6]]$value, 42.07539,     tolerance = 1e-4) # chi-sq
  expect_equal(models[[2]]$statistics[[7]]$value, 1,             tolerance = 1e-6) # df
  expect_equal(models[[2]]$statistics[[8]]$value, 8.782158e-11, tolerance = 1e-4) # p
})
