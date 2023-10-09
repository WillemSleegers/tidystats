# Setup -------------------------------------------------------------------

expected_statistics <- read_stats("../data/effsize.json")

# cohen.d() ---------------------------------------------------------------

test_that("effsize's Cohen's d works", {
  set.seed(1)

  treatment <- rnorm(100, mean = 10)
  control <- rnorm(100, mean = 12)
  d <- c(treatment, control)
  f <- rep(c("Treatment", "Control"), each = 100)

  model <- effsize::cohen.d(d ~ f)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$cohen_d
  )
})

test_that("effsize's Hedges' g works", {
  set.seed(1)

  treatment <- rnorm(100, mean = 10)
  control <- rnorm(100, mean = 12)
  d <- c(treatment, control)
  f <- rep(c("Treatment", "Control"), each = 100)

  model <- effsize::cohen.d(d ~ f, hedges.correction = TRUE)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$hedges_g
  )
})

test_that("effsize's VDA works", {
  set.seed(1)

  treatment <- rnorm(100, mean = 10)
  control <- rnorm(100, mean = 12)
  d <- c(treatment, control)
  f <- rep(c("Treatment", "Control"), each = 100)

  model <- effsize::VD.A(d ~ f)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$vda
  )
})

test_that("effsize's Cliff's delta works", {
  treatment <- c(10, 10, 20, 20, 20, 30, 30, 30, 40, 50)
  control <- c(10, 20, 30, 40, 40, 50)

  model <- effsize::cliff.delta(treatment, control, return.dm = TRUE)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$cliffs_delta
  )
})
