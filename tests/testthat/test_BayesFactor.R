# Setup -------------------------------------------------------------------

if (requireNamespace("BayesFactor", quietly = TRUE)) library(BayesFactor)

# generalTestBF() --------------------------------------------------------

test_that("generalTestBF works", {
  skip_if_not_installed("BayesFactor")
  data(puzzles)

  set.seed(1)

  result <- tidy_stats(generalTestBF(RT ~ shape * color + ID,
    data = puzzles,
    whichRandom = "ID", neverExclude = "ID", progress = FALSE
  ))

  expect_equal(result$method, "Bayesian linear regression")
  models <- result$groups[[1]]$groups
  expect_equal(models[[1]]$name, "shape + ID")
  expect_equal(models[[1]]$statistics[[1]]$value, 320308.9,      tolerance = 0.01) # BF10
  expect_equal(models[[3]]$name, "shape + color + ID")
  expect_equal(models[[3]]$statistics[[1]]$value, 1276619,       tolerance = 0.01) # BF10
  expect_equal(models[[4]]$name, "shape + color + shape:color + ID")
  expect_equal(models[[4]]$statistics[[1]]$value, 512746.9,      tolerance = 0.01) # BF10
})

# lmBF() ------------------------------------------------------------------

test_that("lmBF works", {
  skip_if_not_installed("BayesFactor")
  data(puzzles)

  set.seed(1)

  result <- tidy_stats(lmBF(RT ~ shape + color + shape:color + ID,
    data = puzzles,
    whichRandom = "ID", progress = FALSE
  ))

  expect_equal(result$method, "Bayesian linear regression")
  expect_equal(result$statistics[[1]]$value, 468856.1,       tolerance = 0.01) # BF10
  expect_equal(result$statistics[[2]]$value, 2.132851e-06,   tolerance = 1e-4) # BF01
  expect_equal(result$statistics[[3]]$name, "proportional error")
})

test_that("another lmBF works", {
  skip_if_not_installed("BayesFactor")
  data(puzzles)

  set.seed(1)

  result <- tidy_stats(lmBF(RT ~ shape + color + ID,
    data = puzzles, whichRandom = "ID",
    progress = FALSE
  ))

  expect_equal(result$method, "Bayesian linear regression")
  expect_equal(result$statistics[[1]]$value, 1302102,        tolerance = 0.01) # BF10
  expect_equal(result$statistics[[2]]$value, 7.679892e-07,   tolerance = 1e-4) # BF01
})

test_that("lmBF division works", {
  skip_if_not_installed("BayesFactor")
  data(puzzles)

  set.seed(1)

  bfFull <- lmBF(RT ~ shape + color + shape:color + ID,
    data = puzzles,
    whichRandom = "ID", progress = FALSE
  )

  set.seed(1)

  bfMain <- lmBF(RT ~ shape + color + ID,
    data = puzzles, whichRandom = "ID",
    progress = FALSE
  )

  result <- tidy_stats(bfMain / bfFull)

  expect_equal(result$method, "Bayesian linear regression")
  expect_equal(result$statistics[[1]]$value, 2.777188,    tolerance = 1e-3) # BF10
  expect_equal(result$statistics[[2]]$value, 0.3600764,   tolerance = 1e-4) # BF01
})

# regressionBF() ----------------------------------------------------------

test_that("regressionBF works", {
  skip_if_not_installed("BayesFactor")
  data(attitude)

  result <- tidy_stats(regressionBF(rating ~ ., data = attitude, progress = FALSE))

  expect_equal(result$method, "Bayesian linear regression")
  models <- result$groups[[1]]$groups
  expect_equal(length(models), 63)
  expect_equal(models[[1]]$name, "complaints")
  expect_equal(models[[1]]$statistics[[1]]$value, 417938.6,      tolerance = 0.01) # BF10
  expect_equal(models[[2]]$name, "privileges")
  expect_equal(models[[2]]$statistics[[1]]$value, 3.177784,      tolerance = 1e-3) # BF10
})

test_that("best regressionBF works", {
  skip_if_not_installed("BayesFactor")
  data(attitude)

  attitudeBF <- regressionBF(rating ~ ., data = attitude, progress = FALSE)
  result <- tidy_stats(attitudeBF / attitudeBF[63])

  expect_equal(result$method, "Bayesian linear regression")
  models <- result$groups[[1]]$groups
  expect_equal(length(models), 63)
  expect_equal(models[[1]]$name, "complaints")
  expect_equal(models[[1]]$statistics[[1]]$value, 188.949,    tolerance = 1e-2) # BF10
  expect_equal(models[[1]]$statistics[[2]]$value, 0.005292433, tolerance = 1e-4) # BF01
})

# ttestBF() ---------------------------------------------------------------

test_that("ttestBF works", {
  skip_if_not_installed("BayesFactor")
  result <- tidy_stats(ttestBF(
    x = sleep$extra[sleep$group == 1],
    y = sleep$extra[sleep$group == 2], paired = TRUE
  ))

  expect_equal(result$method, "Bayesian t-test")
  expect_equal(result$statistics[[1]]$value, 17.25888,       tolerance = 1e-3) # BF10
  expect_equal(result$statistics[[2]]$value, 0.05794119,     tolerance = 1e-4) # BF01
  expect_equal(result$statistics[[3]]$name, "proportional error")
})

# anovaBF() ---------------------------------------------------------------

test_that("anovaBF works", {
  skip_if_not_installed("BayesFactor")
  set.seed(1)

  result <- tidy_stats(anovaBF(extra ~ group + ID,
    data = sleep, whichRandom = "ID",
    progress = FALSE
  ))

  expect_equal(result$method, "Bayesian linear regression")
  expect_equal(result$statistics[[1]]$value, 11.67438,    tolerance = 1e-3) # BF10
  expect_equal(result$statistics[[2]]$value, 0.08565764,  tolerance = 1e-4) # BF01
  expect_equal(result$statistics[[3]]$name, "proportional error")
})

test_that("another anovaBF works", {
  skip_if_not_installed("BayesFactor")
  data(puzzles)

  set.seed(1)

  result <- tidy_stats(anovaBF(RT ~ shape * color + ID,
    data = puzzles,
    whichRandom = "ID", whichModels = "top", progress = FALSE
  ))

  expect_equal(result$method, "Bayesian linear regression")
  models <- result$groups[[1]]$groups
  expect_equal(models[[1]]$name, "shape + color + ID")
  expect_equal(models[[1]]$statistics[[1]]$value, 2.689314,   tolerance = 1e-3) # BF10
  expect_equal(models[[2]]$name, "shape + shape:color + ID")
  expect_equal(models[[2]]$statistics[[1]]$value, 0.243448,   tolerance = 1e-3) # BF10
})

# correlationBF() ---------------------------------------------------------

test_that("correlationBF works", {
  skip_if_not_installed("BayesFactor")
  result <- tidy_stats(correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width))

  expect_equal(result$method, "Bayesian correlation")
  expect_equal(result$statistics[[1]]$value, 0.5090175,   tolerance = 1e-4) # BF10
  expect_equal(result$statistics[[2]]$value, 1.964569,    tolerance = 1e-3) # BF01
})

# contingencyTableBF() ----------------------------------------------------

test_that("contingencyTableBF works", {
  skip_if_not_installed("BayesFactor")
  data(raceDolls)

  set.seed(1)

  result <- tidy_stats(contingencyTableBF(raceDolls,
    sampleType = "indepMulti",
    fixedMargin = "cols"
  ))

  expect_equal(result$method, "Bayesian contingency table")
  expect_equal(result$statistics[[1]]$value, 1.814856,   tolerance = 1e-3) # BF10
  expect_equal(result$statistics[[2]]$value, 0.551008,   tolerance = 1e-4) # BF01
})

# proportionBF() ----------------------------------------------------------

test_that("proportionBF works", {
  skip_if_not_installed("BayesFactor")
  result <- tidy_stats(proportionBF(y = 15, N = 25, p = .5))

  expect_equal(result$method, "Bayesian analysis of proportions")
  expect_equal(result$statistics[[1]]$value, 0.6598725,   tolerance = 1e-4) # BF10
  expect_equal(result$statistics[[2]]$value, 1.515444,    tolerance = 1e-3) # BF01
})

# meta.ttestBF() ----------------------------------------------------------

test_that("meta.ttestBF works", {
  skip_if_not_installed("BayesFactor")
  t <- c(-0.15, 2.39, 2.42, 2.43)
  N <- c(100, 150, 97, 99)

  result <- tidy_stats(meta.ttestBF(t, N, rscale = 1, nullInterval = c(0, Inf)))

  expect_equal(result$method, "Bayesian meta-analysis")
  models <- result$groups[[1]]$groups
  expect_equal(models[[1]]$name, "Alt., r=1 0<d<Inf")
  expect_equal(models[[1]]$statistics[[1]]$value, 38.68248,      tolerance = 1e-3) # BF10
  expect_equal(models[[1]]$statistics[[2]]$value, 0.0258515,     tolerance = 1e-4) # BF01
  expect_equal(models[[2]]$name, "Alt., r=1 !(0<d<Inf)")
  expect_equal(models[[2]]$statistics[[1]]$value, 0.008033391,   tolerance = 1e-5) # BF10
})
