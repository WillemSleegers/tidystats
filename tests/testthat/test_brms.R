# brm() -------------------------------------------------------------------

test_that("brmsfit extracts correct statistics", {
  skip_if_not_installed("brms")
  skip_if_not_installed("lme4")
  skip_on_cran()

  model <- brms::brm(
    Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy,
    iter = 2000,
    chains = 4,
    seed = 42,
    file = test_path("brms_sleepstudy"),
    silent = 2,
    refresh = 0
  )

  result <- tidy_stats(model)

  expect_equal(result$method, "Bayesian regression model")

  # Group-Level Effects come first, then Population-Level Effects
  pop_group <- result$groups[[2]]
  expect_equal(pop_group$name, "Population-Level Effects")

  # Intercept (estimate ~ 250 for sleepstudy)
  intercept <- pop_group$groups[[1]]
  expect_equal(intercept$name, "Intercept")
  expect_equal(intercept$statistics[[1]]$value, 251, tolerance = 5) # estimate
  expect_equal(intercept$statistics[[1]]$lower,  240, tolerance = 5) # lower CI
  expect_equal(intercept$statistics[[1]]$upper,  263, tolerance = 5) # upper CI
  expect_equal(intercept$statistics[[3]]$name, "R-hat")
  expect_equal(intercept$statistics[[3]]$value, 1, tolerance = 0.02)  # convergence

  # Days (estimate ~ 10.5)
  days <- pop_group$groups[[2]]
  expect_equal(days$name, "Days")
  expect_equal(days$statistics[[1]]$value, 10.5, tolerance = 1) # estimate
  expect_equal(days$statistics[[3]]$value,  1,   tolerance = 0.02) # R-hat
})

test_that("brmsfit with custom prob extracts correct CI", {
  skip_if_not_installed("brms")
  skip_if_not_installed("lme4")
  skip_on_cran()

  model <- brms::brm(
    Reaction ~ Days,
    data = lme4::sleepstudy,
    iter = 2000,
    chains = 4,
    seed = 42,
    file = test_path("brms_sleepstudy_simple"),
    silent = 2,
    refresh = 0
  )

  result <- tidy_stats(model, args = list(prob = 0.89))

  expect_equal(result$method, "Bayesian regression model")
  pop_group <- result$groups[[1]]
  intercept <- pop_group$groups[[1]]
  expect_equal(intercept$statistics[[1]]$level, 0.89)
})
