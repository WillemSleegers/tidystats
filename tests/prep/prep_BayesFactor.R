# Setup -------------------------------------------------------------------

library(BayesFactor)

statistics <- list()

# generalTestBF() ---------------------------------------------------------

set.seed(1)

data(puzzles)

generalTestBF <- generalTestBF(
  RT ~ shape * color + ID,
  data = puzzles,
  whichRandom = "ID",
  neverExclude = "ID",
  progress = FALSE
)

statistics <- add_stats(statistics, generalTestBF)

generalTestBF

# lmBF() ------------------------------------------------------------------

set.seed(1)

data(puzzles)

bfFull <- lmBF(
  RT ~ shape + color + shape:color + ID,
  data = puzzles,
  whichRandom = "ID"
)

set.seed(1)

bfMain <- lmBF(RT ~ shape + color + ID, data = puzzles, whichRandom = "ID")
bfMainFull <- bfMain / bfFull

statistics <- statistics |>
  add_stats(bfFull) |>
  add_stats(bfMain) |>
  add_stats(bfMainFull)

bfFull
bfMain
bfMainFull

# regressionBF() ----------------------------------------------------------

set.seed(1)

data(attitude)

attitudeBF <- regressionBF(rating ~ ., data = attitude, progress = FALSE)
attitudeBFBest <- attitudeBF / attitudeBF[63]

statistics <- statistics |>
  add_stats(attitudeBF) |>
  add_stats(attitudeBFBest)

attitudeBF
attitudeBFBest

# ttestBF() ---------------------------------------------------------------

set.seed(1)

diffScores <- sleep$extra[1:10] - sleep$extra[11:20]

sleepTTestBF <- ttestBF(
  x = sleep$extra[sleep$group == 1],
  y = sleep$extra[sleep$group == 2],
  paired = TRUE
)
sleepTTestBF_interval <- ttestBF(x = diffScores, nullInterval = c(-Inf, 0))

statistics <- statistics |>
  add_stats(sleepTTestBF) |>
  add_stats(sleepTTestBF_interval)

sleepTTestBF
sleepTTestBF_interval

# anovaBF() ---------------------------------------------------------------

set.seed(1)

data(puzzles)

sleepAnovaBF <- anovaBF(
  extra ~ group + ID,
  data = sleep,
  whichRandom = "ID",
  progress = FALSE
)

set.seed(1)

puzzlesAnovaBF <- anovaBF(
  RT ~ shape * color + ID,
  data = puzzles,
  whichRandom = "ID",
  whichModels = "top",
  progress = FALSE
)

statistics <- statistics |>
  add_stats(sleepAnovaBF) |>
  add_stats(puzzlesAnovaBF)

sleepAnovaBF
puzzlesAnovaBF

# correlationBF() ---------------------------------------------------------

set.seed(1)

correlationBF <- correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)

statistics <- add_stats(statistics, correlationBF)

correlationBF

# contingencyTableBF() ----------------------------------------------------

set.seed(1)

data(raceDolls)

contingencyTableBF <- contingencyTableBF(
  raceDolls,
  sampleType = "indepMulti",
  fixedMargin = "cols"
)

statistics <- add_stats(statistics, contingencyTableBF)

contingencyTableBF

# proportionBF() ----------------------------------------------------------

set.seed(1)

proportionBF <- proportionBF(y = 15, N = 25, p = .5)

statistics <- add_stats(statistics, proportionBF)

proportionBF

# meta.ttestBF() ----------------------------------------------------------

set.seed(1)

t <- c(-.15, 2.39, 2.42, 2.43)
N <- c(100, 150, 97, 99)

metaBF <- meta.ttestBF(t, N, rscale = 1, nullInterval = c(0, Inf))

statistics <- add_stats(statistics, metaBF)

metaBF

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/BayesFactor.json")

# Cleanup -----------------------------------------------------------------

rm(
  attitude, attitudeBF, attitudeBFBest, bfFull, bfMain, bfMainFull,
  contingencyTableBF, correlationBF, generalTestBF, metaBF, proportionBF,
  puzzles, puzzlesAnovaBF, raceDolls, sleepAnovaBF, sleepTTestBF,
  sleepTTestBF_interval, diffScores, N, t, df, statistics
)
