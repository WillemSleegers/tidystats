# Setup -------------------------------------------------------------------

if (requireNamespace("BayesFactor", quietly = TRUE)) library(BayesFactor)

# Compare against the Bayes factors the object itself reports (via
# extractBF()) rather than hard-coded constants. BayesFactor computes several
# of these via Monte Carlo integration, so the exact values can change across
# package versions; tidy_stats only needs to surface whatever extractBF()
# returns.

# generalTestBF() --------------------------------------------------------

test_that("generalTestBF works", {
  skip_if_not_installed("BayesFactor")
  data(puzzles)

  set.seed(1)

  bf <- generalTestBF(RT ~ shape * color + ID,
    data = puzzles,
    whichRandom = "ID", neverExclude = "ID", progress = FALSE
  )
  result <- tidy_stats(bf)
  bf_table <- extractBF(bf)

  expect_equal(result$method, "Bayesian linear regression")
  models <- result$groups[[1]]$groups
  for (i in seq_len(nrow(bf_table))) {
    expect_equal(models[[i]]$name, rownames(bf_table)[i])
    expect_equal(models[[i]]$statistics[[1]]$value, bf_table$bf[i]) # BF10
  }
})

# lmBF() ------------------------------------------------------------------

test_that("lmBF works", {
  skip_if_not_installed("BayesFactor")
  data(puzzles)

  set.seed(1)

  bf <- lmBF(RT ~ shape + color + shape:color + ID,
    data = puzzles,
    whichRandom = "ID", progress = FALSE
  )
  result <- tidy_stats(bf)
  bf_table <- extractBF(bf)

  expect_equal(result$method, "Bayesian linear regression")
  expect_equal(result$statistics[[1]]$value, bf_table$bf)     # BF10
  expect_equal(result$statistics[[2]]$value, 1 / bf_table$bf) # BF01
  expect_equal(result$statistics[[3]]$name, "proportional error")
})

test_that("another lmBF works", {
  skip_if_not_installed("BayesFactor")
  data(puzzles)

  set.seed(1)

  bf <- lmBF(RT ~ shape + color + ID,
    data = puzzles, whichRandom = "ID",
    progress = FALSE
  )
  result <- tidy_stats(bf)
  bf_table <- extractBF(bf)

  expect_equal(result$method, "Bayesian linear regression")
  expect_equal(result$statistics[[1]]$value, bf_table$bf)     # BF10
  expect_equal(result$statistics[[2]]$value, 1 / bf_table$bf) # BF01
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

  ratio <- bfMain / bfFull
  result <- tidy_stats(ratio)
  bf_table <- extractBF(ratio)

  expect_equal(result$method, "Bayesian linear regression")
  expect_equal(result$statistics[[1]]$value, bf_table$bf)     # BF10
  expect_equal(result$statistics[[2]]$value, 1 / bf_table$bf) # BF01
})

# regressionBF() ----------------------------------------------------------

test_that("regressionBF works", {
  skip_if_not_installed("BayesFactor")
  data(attitude)

  bf <- regressionBF(rating ~ ., data = attitude, progress = FALSE)
  result <- tidy_stats(bf)
  bf_table <- extractBF(bf)

  expect_equal(result$method, "Bayesian linear regression")
  models <- result$groups[[1]]$groups
  expect_equal(length(models), 63)
  for (i in seq_len(nrow(bf_table))) {
    expect_equal(models[[i]]$name, rownames(bf_table)[i])
    expect_equal(models[[i]]$statistics[[1]]$value, bf_table$bf[i]) # BF10
  }
})

test_that("best regressionBF works", {
  skip_if_not_installed("BayesFactor")
  data(attitude)

  attitudeBF <- regressionBF(rating ~ ., data = attitude, progress = FALSE)
  ratio <- attitudeBF / attitudeBF[63]
  result <- tidy_stats(ratio)
  bf_table <- extractBF(ratio)

  expect_equal(result$method, "Bayesian linear regression")
  models <- result$groups[[1]]$groups
  expect_equal(length(models), 63)
  for (i in seq_len(nrow(bf_table))) {
    expect_equal(models[[i]]$name, rownames(bf_table)[i])
    expect_equal(models[[i]]$statistics[[1]]$value, bf_table$bf[i])     # BF10
    expect_equal(models[[i]]$statistics[[2]]$value, 1 / bf_table$bf[i]) # BF01
  }
})

# ttestBF() ---------------------------------------------------------------

test_that("ttestBF works", {
  skip_if_not_installed("BayesFactor")
  bf <- ttestBF(
    x = sleep$extra[sleep$group == 1],
    y = sleep$extra[sleep$group == 2], paired = TRUE
  )
  result <- tidy_stats(bf)
  bf_table <- extractBF(bf)

  expect_equal(result$method, "Bayesian t-test")
  expect_equal(result$statistics[[1]]$value, bf_table$bf)     # BF10
  expect_equal(result$statistics[[2]]$value, 1 / bf_table$bf) # BF01
  expect_equal(result$statistics[[3]]$name, "proportional error")
})

# anovaBF() ---------------------------------------------------------------

test_that("anovaBF works", {
  skip_if_not_installed("BayesFactor")
  set.seed(1)

  bf <- anovaBF(extra ~ group + ID,
    data = sleep, whichRandom = "ID",
    progress = FALSE
  )
  result <- tidy_stats(bf)
  bf_table <- extractBF(bf)

  expect_equal(result$method, "Bayesian linear regression")
  expect_equal(result$statistics[[1]]$value, bf_table$bf)     # BF10
  expect_equal(result$statistics[[2]]$value, 1 / bf_table$bf) # BF01
  expect_equal(result$statistics[[3]]$name, "proportional error")
})

test_that("another anovaBF works", {
  skip_if_not_installed("BayesFactor")
  data(puzzles)

  set.seed(1)

  bf <- anovaBF(RT ~ shape * color + ID,
    data = puzzles,
    whichRandom = "ID", whichModels = "top", progress = FALSE
  )
  result <- tidy_stats(bf)
  bf_table <- extractBF(bf)

  expect_equal(result$method, "Bayesian linear regression")
  models <- result$groups[[1]]$groups
  for (i in seq_len(nrow(bf_table))) {
    expect_equal(models[[i]]$name, rownames(bf_table)[i])
    expect_equal(models[[i]]$statistics[[1]]$value, bf_table$bf[i]) # BF10
  }
})

# correlationBF() ---------------------------------------------------------

test_that("correlationBF works", {
  skip_if_not_installed("BayesFactor")
  bf <- correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)
  result <- tidy_stats(bf)
  bf_table <- extractBF(bf)

  expect_equal(result$method, "Bayesian correlation")
  expect_equal(result$statistics[[1]]$value, bf_table$bf)     # BF10
  expect_equal(result$statistics[[2]]$value, 1 / bf_table$bf) # BF01
})

# contingencyTableBF() ----------------------------------------------------

test_that("contingencyTableBF works", {
  skip_if_not_installed("BayesFactor")
  data(raceDolls)

  set.seed(1)

  bf <- contingencyTableBF(raceDolls,
    sampleType = "indepMulti",
    fixedMargin = "cols"
  )
  result <- tidy_stats(bf)
  bf_table <- extractBF(bf)

  expect_equal(result$method, "Bayesian contingency table")
  expect_equal(result$statistics[[1]]$value, bf_table$bf)     # BF10
  expect_equal(result$statistics[[2]]$value, 1 / bf_table$bf) # BF01
})

# proportionBF() ----------------------------------------------------------

test_that("proportionBF works", {
  skip_if_not_installed("BayesFactor")
  bf <- proportionBF(y = 15, N = 25, p = .5)
  result <- tidy_stats(bf)
  bf_table <- extractBF(bf)

  expect_equal(result$method, "Bayesian analysis of proportions")
  expect_equal(result$statistics[[1]]$value, bf_table$bf)     # BF10
  expect_equal(result$statistics[[2]]$value, 1 / bf_table$bf) # BF01
})

# meta.ttestBF() ----------------------------------------------------------

test_that("meta.ttestBF works", {
  skip_if_not_installed("BayesFactor")
  t <- c(-0.15, 2.39, 2.42, 2.43)
  N <- c(100, 150, 97, 99)

  bf <- meta.ttestBF(t, N, rscale = 1, nullInterval = c(0, Inf))
  result <- tidy_stats(bf)
  bf_table <- extractBF(bf)

  expect_equal(result$method, "Bayesian meta-analysis")
  models <- result$groups[[1]]$groups
  for (i in seq_len(nrow(bf_table))) {
    expect_equal(models[[i]]$name, rownames(bf_table)[i])
    expect_equal(models[[i]]$statistics[[1]]$value, bf_table$bf[i]) # BF10
  }
})
