
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# BayesFactor -------------------------------------------------------------

# Load package
library(BayesFactor)

# Load data
data(puzzles)
data(attitude)
data(raceDolls)

## Bem's (2010) data (see Rouder & Morey, 2011)
t = c(-.15,2.39,2.42,2.43)
N = c(100,150,97,99)

# generalTestBF ----------------------------------------------

# Run model
set.seed(1)
generalTestBF <- generalTestBF(RT ~ shape*color + ID, data = puzzles, 
  whichRandom = "ID", neverExclude = "ID", progress = FALSE)
generalTestBF

# Tidy stats
temp <- tidy_stats(generalTestBF)

# lmBF -------------------------------------------------------

# Run models
set.seed(1)
bfFull <- lmBF(RT ~ shape + color + shape:color + ID, data = puzzles, 
  whichRandom = "ID", )
bfFull

set.seed(1)
bfMain <- lmBF(RT ~ shape + color + ID, data = puzzles, whichRandom = "ID")
bfMain

set.seed(1)
bfMainFull <- bfMain / bfFull
bfMainFull

# Tidy stats
temp <- tidy_stats(bfFull)
temp <- tidy_stats(bfMain)
temp <- tidy_stats(bfMainFull)

# regressionBF -----------------------------------------------

attitudeBF <- regressionBF(rating ~ ., data = attitude, progress = FALSE)
head(attitudeBF)

attitudeBFBest <- attitudeBF / attitudeBF[63]
head(attitudeBFBest)

temp <- tidy_stats(attitudeBF)
temp <- tidy_stats(attitudeBFBest)

# ttestBF ----------------------------------------------------

# Run models
sleepTTestBF <- ttestBF(x = sleep$extra[sleep$group == 1], 
  y = sleep$extra[sleep$group == 2], paired = TRUE)
sleepTTestBF

# Tidy stats
temp <- tidy_stats(sleepTTestBF)

# anovaBF ----------------------------------------------------

sleepAnovaBF <- anovaBF(extra ~ group + ID, data = sleep, whichRandom = "ID",
    progress = FALSE)
sleepAnovaBF

puzzlesAnovaBF = anovaBF(RT ~ shape * color + ID, data = puzzles, 
  whichRandom = "ID", whichModels = 'top', progress = FALSE)
puzzlesAnovaBF

# Tidy stats
temp <- tidy_stats(sleepAnovaBF)
temp <- tidy_stats(puzzlesAnovaBF)

# correlationBF ----------------------------------------------

# Run models
correlationBF = correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)
correlationBF

# Tidy stats
temp <- tidy_stats(correlationBF)

# contingencyTableBF -----------------------------------------

# Run model
contingencyTableBF = contingencyTableBF(raceDolls, sampleType = "indepMulti", 
  fixedMargin = "cols")
contingencyTableBF

# Tidy stats
temp <- tidy_stats(contingencyTableBF)

# proportionBF -----------------------------------------------

# Run models
proportionBF = proportionBF(y = 15, N = 25, p = .5)
proportionBF

# Tidy stats
temp <- tidy_stats(proportionBF)

# meta.ttestBF -----------------------------------------------

# Run models
metaBF = meta.ttestBF(t, N, rscale = 1, nullInterval = c(0, Inf))
metaBF

# Tidy stats
temp <- tidy_stats(metaBF)

# add_stats() -------------------------------------------------------------

# Create an empty list
results <- list()

# Add stats
results <- results %>%
  add_stats(generalTestBF) %>%
  add_stats(bfFull) %>%
  add_stats(bfMain) %>%
  add_stats(bfMainFull) %>%
  add_stats(attitudeBF) %>%
  add_stats(attitudeBFBest) %>%
  add_stats(sleepTTestBF) %>%
  add_stats(sleepAnovaBF) %>%
  add_stats(puzzlesAnovaBF) %>%
  add_stats(correlationBF) %>%
  add_stats(contingencyTableBF) %>%
  add_stats(proportionBF) %>%
  add_stats(metaBF)

# Save stats
write_stats(results, "inst/test_data/BayesFactor.json")
