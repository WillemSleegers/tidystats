# Setup -------------------------------------------------------------------

library(BayesFactor)

path <- system.file("tests/data/BayesFactor.json", package = "tidystats")
expected_statistics <- read_stats(path)

# generalTestBF() --------------------------------------------------------

test_that("generalTestBF works", {
  data(puzzles)

  set.seed(1)

  model <- generalTestBF(RT ~ shape * color + ID,
    data = puzzles,
    whichRandom = "ID", neverExclude = "ID", progress = FALSE
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$generalTestBF
  )
})

# lmBF() ------------------------------------------------------------------

test_that("lmBF works", {
  data(puzzles)

  set.seed(1)

  model <- lmBF(RT ~ shape + color + shape:color + ID,
    data = puzzles,
    whichRandom = "ID", progress = FALSE
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$bfFull
  )
})

test_that("another lmBF works", {
  data(puzzles)

  set.seed(1)

  model <- lmBF(RT ~ shape + color + ID,
    data = puzzles, whichRandom = "ID",
    progress = FALSE
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$bfMain
  )
})

test_that("lmBF division works", {
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

  model <- bfMain / bfFull

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$bfMainFull
  )
})

# regressionBF() ----------------------------------------------------------

test_that("regressionBF works", {
  data(attitude)

  model <- regressionBF(rating ~ ., data = attitude, progress = FALSE)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$attitudeBF
  )
})

test_that("best regressionBF works", {
  data(attitude)

  attitudeBF <- regressionBF(rating ~ ., data = attitude, progress = FALSE)
  model <- attitudeBF / attitudeBF[63]

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$attitudeBFBest
  )
})

# ttestBF() ---------------------------------------------------------------

test_that("ttestBF works", {
  model <- ttestBF(
    x = sleep$extra[sleep$group == 1],
    y = sleep$extra[sleep$group == 2], paired = TRUE
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$sleepTTestBF
  )
})

# anovaBF() ---------------------------------------------------------------

test_that("anovaBF works", {
  set.seed(1)

  model <- anovaBF(extra ~ group + ID,
    data = sleep, whichRandom = "ID",
    progress = FALSE
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$sleepAnovaBF
  )
})

test_that("another anovaBF works", {
  data(puzzles)

  set.seed(1)

  model <- anovaBF(RT ~ shape * color + ID,
    data = puzzles,
    whichRandom = "ID", whichModels = "top", progress = FALSE
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$puzzlesAnovaBF
  )
})

# correlationBF() ---------------------------------------------------------

test_that("correlationBF works", {
  model <- correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$correlationBF
  )
})

# contingencyTableBF() ----------------------------------------------------

test_that("contingencyTableBF works", {
  data(raceDolls)

  set.seed(1)

  model <- contingencyTableBF(raceDolls,
    sampleType = "indepMulti",
    fixedMargin = "cols"
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$contingencyTableBF
  )
})

# proportionBF() ----------------------------------------------------------

test_that("proportionBF works", {
  model <- proportionBF(y = 15, N = 25, p = .5)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$proportionBF
  )
})

# meta.ttestBF() ----------------------------------------------------------

test_that("meta.ttestBF works", {
  t <- c(-0.15, 2.39, 2.42, 2.43)
  N <- c(100, 150, 97, 99)

  model <- meta.ttestBF(t, N, rscale = 1, nullInterval = c(0, Inf))

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$metaBF
  )
})
