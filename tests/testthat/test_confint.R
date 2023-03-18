# Setup -------------------------------------------------------------------

path <- system.file("tests/data/confint.json", package = "tidystats")
expected_statistics <- read_stats(path)

# lm() --------------------------------------------------------------------

test_that("confint works", {
  fit <- lm(100 / mpg ~ disp + hp + wt + am, data = mtcars)
  model <- confint(fit)

  class(model) <- append(class(model), "confint", after = 0)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$CI_fit
  )
})

test_that("single coefficient confint works", {
  fit <- lm(100 / mpg ~ disp + hp + wt + am, data = mtcars)
  model <- confint(fit, "wt")

  class(model) <- append(class(model), "confint", after = 0)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$CI_fit_wt
  )
})

test_that("profile likelihood confint works", {
  D93 <- tibble::tibble(
    counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
    outcome = gl(3, 1, 9),
    treatment = gl(3, 3)
  )

  fit <- glm(counts ~ outcome + treatment, data = D93, family = poisson())
  model <- confint(fit)

  class(model) <- append(class(model), "confint", after = 0)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$CI_glm_D93_MASS
  )
})

test_that("asymptotic normality confint works", {
  D93 <- tibble::tibble(
    counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
    outcome = gl(3, 1, 9),
    treatment = gl(3, 3)
  )

  fit <- glm(counts ~ outcome + treatment, data = D93, family = poisson())
  model <- confint.default(fit)

  class(model) <- append(class(model), "confint", after = 0)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$CI_glm_D93_default
  )
})
