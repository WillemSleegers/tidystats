# confint() ---------------------------------------------------------------

test_that("confint works", {
  fit <- lm(100 / mpg ~ disp + hp + wt + am, data = mtcars)
  model <- confint(fit)

  class(model) <- append(class(model), "confint", after = 0)

  result <- tidy_stats(model)

  expect_equal(result$method, "Confidence intervals")

  coefs <- result$groups[[1]]$groups
  expect_equal(coefs[[1]]$name, "(Intercept)")
  expect_equal(coefs[[4]]$name, "wt")
  expect_equal(coefs[[4]]$statistics[[1]]$value, 0.3800887, tolerance = 1e-4) # lower
  expect_equal(coefs[[4]]$statistics[[2]]$value, 1.622518,  tolerance = 1e-4) # upper
})

test_that("single coefficient confint works", {
  fit <- lm(100 / mpg ~ disp + hp + wt + am, data = mtcars)
  model <- confint(fit, "wt")

  class(model) <- append(class(model), "confint", after = 0)

  result <- tidy_stats(model)

  coefs <- result$groups[[1]]$groups
  expect_equal(coefs[[1]]$name, "wt")
  expect_equal(coefs[[1]]$statistics[[1]]$value, 0.3800887, tolerance = 1e-4) # lower
  expect_equal(coefs[[1]]$statistics[[2]]$value, 1.622518,  tolerance = 1e-4) # upper
})

test_that("profile likelihood confint works", {
  D93 <- data.frame(
    counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
    outcome = gl(3, 1, 9),
    treatment = gl(3, 3)
  )

  fit <- glm(counts ~ outcome + treatment, data = D93, family = poisson())
  model <- suppressMessages(confint(fit))

  class(model) <- append(class(model), "confint", after = 0)

  result <- tidy_stats(model)

  coefs <- result$groups[[1]]$groups
  expect_equal(coefs[[1]]$name, "(Intercept)")
  expect_equal(coefs[[1]]$statistics[[1]]$value, 2.695822, tolerance = 1e-4) # lower
  expect_equal(coefs[[1]]$statistics[[2]]$value, 3.366556, tolerance = 1e-4) # upper
  expect_equal(coefs[[2]]$name, "outcome2")
  expect_equal(coefs[[2]]$statistics[[1]]$value, -0.8577018, tolerance = 1e-4) # lower
})

test_that("asymptotic normality confint works", {
  D93 <- data.frame(
    counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
    outcome = gl(3, 1, 9),
    treatment = gl(3, 3)
  )

  fit <- glm(counts ~ outcome + treatment, data = D93, family = poisson())
  model <- confint.default(fit)

  class(model) <- append(class(model), "confint", after = 0)

  result <- tidy_stats(model)

  coefs <- result$groups[[1]]$groups
  expect_equal(coefs[[1]]$name, "(Intercept)")
  expect_equal(coefs[[1]]$statistics[[1]]$value, 2.709567, tolerance = 1e-4) # lower
  expect_equal(coefs[[1]]$statistics[[2]]$value, 3.379478, tolerance = 1e-4) # upper
})
