
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(BayesFactor)

# Create an empty list
results <- list()

# generalTestBF() ---------------------------------------------------------

# Set seed
set.seed(1)

# Get data
data(puzzles)

# Run analyses
generalTestBF <- generalTestBF(RT ~ shape*color + ID, data = puzzles, 
  whichRandom = "ID", neverExclude = "ID", progress = FALSE)

# Add stats
results <- add_stats(results, generalTestBF)

# Inspect output
generalTestBF

# lmBF() ------------------------------------------------------------------

# Set seed
set.seed(1)

# Get data
data(puzzles)

# Run analyses
bfFull <- lmBF(RT ~ shape + color + shape:color + ID, data = puzzles, 
  whichRandom = "ID", )
bfMain <- lmBF(RT ~ shape + color + ID, data = puzzles, whichRandom = "ID")
bfMainFull <- bfMain / bfFull

# Add stats
results <- results %>%
  add_stats(bfFull) %>%
  add_stats(bfMain) %>%
  add_stats(bfMainFull)

# Inspect output
bfFull
bfMain
bfMainFull

# regressionBF() ----------------------------------------------------------

# Set seed
set.seed(1)

# Get data
data(attitude)

# Run analyses
attitudeBF <- regressionBF(rating ~ ., data = attitude, progress = FALSE)
attitudeBFBest <- attitudeBF / attitudeBF[63]

# Add stats
results <- results %>%
  add_stats(attitudeBF) %>%
  add_stats(attitudeBFBest)

# Inspect output
attitudeBF
attitudeBFBest

# ttestBF() ---------------------------------------------------------------

# Set seed
set.seed(1)

# Get data
diffScores <- sleep$extra[1:10] - sleep$extra[11:20]

# Run analyses
sleepTTestBF <- ttestBF(x = sleep$extra[sleep$group == 1], 
  y = sleep$extra[sleep$group == 2], paired = TRUE)
sleepTTestBF_interval <- ttestBF(x = diffScores, nullInterval=c(-Inf,0))

# Add stats
results <- results %>%
  add_stats(sleepTTestBF) %>%
  add_stats(sleepTTestBF_interval)

# Inspect output
sleepTTestBF
sleepTTestBF_interval

# anovaBF() ---------------------------------------------------------------

# Set seed
set.seed(1)

# Get data
data(puzzles)

# Run analyses
sleepAnovaBF <- anovaBF(extra ~ group + ID, data = sleep, whichRandom = "ID",
    progress = FALSE)
puzzlesAnovaBF = anovaBF(RT ~ shape * color + ID, data = puzzles, 
  whichRandom = "ID", whichModels = 'top', progress = FALSE)

# Add stats
results <- results %>%
  add_stats(sleepAnovaBF) %>%
  add_stats(puzzlesAnovaBF)

# Inspect output
sleepAnovaBF
puzzlesAnovaBF

# correlationBF() ---------------------------------------------------------

# Set seed
set.seed(1)

# Run analysis
correlationBF <- correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)

# Add stats
results <- add_stats(results, correlationBF)

# Inspect output
correlationBF

# contingencyTableBF() ----------------------------------------------------

# Set seed
set.seed(1)

# Get data
data(raceDolls)

# Run analysis
contingencyTableBF <- contingencyTableBF(raceDolls, sampleType = "indepMulti", 
  fixedMargin = "cols")

# Add stats
results <- add_stats(results, contingencyTableBF)

# Inspect output
contingencyTableBF

# proportionBF() ----------------------------------------------------------

# Set seed
set.seed(1)

# Run analysis
proportionBF <- proportionBF(y = 15, N = 25, p = .5)

# Add stats
results <- add_stats(results, proportionBF)

# Inspect output
proportionBF

# meta.ttestBF() ----------------------------------------------------------

# Set seed
set.seed(1)

# Get data
t <- c(-.15, 2.39, 2.42, 2.43)
N <- c(100, 150, 97, 99)

# Run analysis
metaBF <- meta.ttestBF(t, N, rscale = 1, nullInterval = c(0, Inf))

# Add stats
results <- add_stats(results, metaBF)

# Inspect output
metaBF

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/BayesFactor.json")

# Cleanup -----------------------------------------------------------------

rm(attitude, attitudeBF, attitudeBFBest, bfFull, bfMain, bfMainFull, 
  contingencyTableBF, correlationBF, generalTestBF, metaBF, proportionBF, 
  puzzles, puzzlesAnovaBF, raceDolls, sleepAnovaBF, sleepTTestBF, 
  sleepTTestBF_interval, diffScores, N, t, df, results)
