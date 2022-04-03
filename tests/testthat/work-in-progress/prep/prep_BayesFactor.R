
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(BayesFactor)

# Load data
data(puzzles)
data(attitude)
data(raceDolls)

## Bem's (2010) data (see Rouder & Morey, 2011)
t <- c(-.15, 2.39, 2.42, 2.43)
N <- c(100, 150, 97, 99)

# Set seed
set.seed(1)

# generalTestBF() ---------------------------------------------------------

# Run test
generalTestBF <- generalTestBF(RT ~ shape*color + ID, data = puzzles, 
  whichRandom = "ID", neverExclude = "ID", progress = FALSE)
generalTestBF

# Tidy stats
temp <- tidy_stats(generalTestBF)

# Add stats
results <- add_stats(results, generalTestBF)

# lmBF() ------------------------------------------------------------------

# Run tests
bfFull <- lmBF(RT ~ shape + color + shape:color + ID, data = puzzles, 
  whichRandom = "ID", )
bfFull

bfMain <- lmBF(RT ~ shape + color + ID, data = puzzles, whichRandom = "ID")
bfMain

bfMainFull <- bfMain / bfFull
bfMainFull

# Tidy stats
temp <- tidy_stats(bfFull)
temp <- tidy_stats(bfMain)
temp <- tidy_stats(bfMainFull)

# Add stats
results <- results %>%
  add_stats(bfFull) %>%
  add_stats(bfMain) %>%
  add_stats(bfMainFull) 

# regressionBF() ----------------------------------------------------------

# Run tests
attitudeBF <- regressionBF(rating ~ ., data = attitude, progress = FALSE)
head(attitudeBF)

attitudeBFBest <- attitudeBF / attitudeBF[63]
head(attitudeBFBest)

# Tidy stats
temp <- tidy_stats(attitudeBF)
temp <- tidy_stats(attitudeBFBest)

# Add stats
results <- results %>%
  add_stats(attitudeBF) %>%
  add_stats(attitudeBFBest)

# ttestBF() ---------------------------------------------------------------

# Get data
diffScores <- sleep$extra[1:10] - sleep$extra[11:20]

# Run analyses
sleepTTestBF <- ttestBF(x = sleep$extra[sleep$group == 1], 
  y = sleep$extra[sleep$group == 2], paired = TRUE)
sleepTTestBF_interval <- ttestBF(x = diffScores, nullInterval=c(-Inf,0))

# Tidy stats
temp <- tidy_stats(sleepTTestBF)
temp <- tidy_stats(sleepTTestBF_interval)

# Add stats
results <- results %>%
  add_stats(sleepTTestBF) %>%
  add_stats(sleepTTestBF_interval)

# anovaBF() ---------------------------------------------------------------

# Run tests
sleepAnovaBF <- anovaBF(extra ~ group + ID, data = sleep, whichRandom = "ID",
    progress = FALSE)
sleepAnovaBF

puzzlesAnovaBF = anovaBF(RT ~ shape * color + ID, data = puzzles, 
  whichRandom = "ID", whichModels = 'top', progress = FALSE)
puzzlesAnovaBF

# Tidy stats
temp <- tidy_stats(sleepAnovaBF)
temp <- tidy_stats(puzzlesAnovaBF)

# Add stats
results <- results %>%
  add_stats(sleepAnovaBF) %>%
  add_stats(puzzlesAnovaBF)

# correlationBF() ---------------------------------------------------------

# Run test
correlationBF = correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)
correlationBF

# Tidy stats
temp <- tidy_stats(correlationBF)

# Add stats
results <- add_stats(results, correlationBF)

# contingencyTableBF() ----------------------------------------------------

# Run test
contingencyTableBF = contingencyTableBF(raceDolls, sampleType = "indepMulti", 
  fixedMargin = "cols")
contingencyTableBF

# Tidy stats
temp <- tidy_stats(contingencyTableBF)

# Add stats
results <- add_stats(results, contingencyTableBF)

# proportionBF() ----------------------------------------------------------

# Run test
proportionBF = proportionBF(y = 15, N = 25, p = .5)
proportionBF

# Tidy stats
temp <- tidy_stats(proportionBF)

# Add stats
results <- add_stats(results, proportionBF)

# meta.ttestBF() ----------------------------------------------------------

# Run test
metaBF = meta.ttestBF(t, N, rscale = 1, nullInterval = c(0, Inf))
metaBF

# Tidy stats
temp <- tidy_stats(metaBF)

# Add stats
results <- add_stats(results, metaBF)

# write_stats() -----------------------------------------------------------

write_stats(results, "inst/test_data/BayesFactor.json")
