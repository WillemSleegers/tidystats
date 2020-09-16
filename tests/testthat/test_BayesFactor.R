
# Setup -------------------------------------------------------------------

# Load test data
test_results <- read_stats(system.file("test_data/BayesFactor.json", 
  package = "tidystats"))

# Set options
tolerance <- 0.001

# Load package
library(BayesFactor)

# Load data
data(puzzles)
data(attitude)
data(raceDolls)

## Bem's (2010) data (see Rouder & Morey, 2011)
t = c(-.15,2.39,2.42,2.43)
N = c(100,150,97,99)

# Test: generalTestBF -----------------------------------------------------

test_that("BayesFactor: generalTestBF works", {
  set.seed(1)
  model <- generalTestBF(RT ~ shape*color + ID, data = puzzles, 
    whichRandom = "ID", neverExclude = "ID", progress = FALSE)
  model
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$generalTestBF
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: lmBF --------------------------------------------------------------

test_that("BayesFactor: lmBF works", {
  set.seed(1)
  model <- lmBF(RT ~ shape + color + shape:color + ID, data = puzzles, 
    whichRandom = "ID", progress = FALSE)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$bfFull
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("BayesFactor: another lmBF works", {
  set.seed(1)
  model <- lmBF(RT ~ shape + color + ID, data = puzzles, whichRandom = "ID",
    progress = FALSE)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$bfMain
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("BayesFactor: lmBF division works", {
  set.seed(1)
  bfFull <- lmBF(RT ~ shape + color + shape:color + ID, data = puzzles, 
    whichRandom = "ID", progress = FALSE)
  set.seed(1)
  bfMain <- lmBF(RT ~ shape + color + ID, data = puzzles, whichRandom = "ID",
    progress = FALSE)
  set.seed(1)
  model <- bfMain / bfFull
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$bfMainFull
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: regressionBF ------------------------------------------------------

test_that("BayesFactor: regressionBF works", {
  model <- regressionBF(rating ~ ., data = attitude, progress = FALSE)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$attitudeBF
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("BayesFactor: best regressionBF works", {
  attitudeBF <- regressionBF(rating ~ ., data = attitude, progress = FALSE)
  model <- attitudeBF / attitudeBF[63]
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$attitudeBFBest
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: ttestBF -----------------------------------------------------------

test_that("BayesFactor: ttestBF works", {
  model <- ttestBF(x = sleep$extra[sleep$group == 1], 
    y = sleep$extra[sleep$group == 2], paired = TRUE)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$sleepTTestBF
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: anovaBF -----------------------------------------------------------

test_that("BayesFactor: anovaBF works", {
  model <- anovaBF(extra ~ group + ID, data = sleep, whichRandom = "ID",
    progress = FALSE)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$sleepAnovaBF
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("BayesFactor: another anovaBF works", {
  model <- anovaBF(RT ~ shape * color + ID, data = puzzles, 
    whichRandom = "ID", whichModels = 'top', progress = FALSE)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$puzzlesAnovaBF
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: correlationBF -----------------------------------------------------

test_that("BayesFactor: correlationBF works", {
  model <- correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$correlationBF
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: contingencyTableBF ------------------------------------------------

test_that("BayesFactor: contingencyTableBF works", {
  model <- contingencyTableBF(raceDolls, sampleType = "indepMulti", 
    fixedMargin = "cols")
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$contingencyTableBF
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: proportionBF ------------------------------------------------------

test_that("BayesFactor: proportionBF works", {
  model <- proportionBF(y = 15, N = 25, p = .5)
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$proportionBF
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: meta.ttestBF ------------------------------------------------------

test_that("BayesFactor: meta.ttestBF works", {
  model <- meta.ttestBF(t, N, rscale = 1, nullInterval = c(0, Inf))
  
  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$metaBF
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
