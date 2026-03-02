# Setup -------------------------------------------------------------------

library(effsize)

statistics <- list()

# cohen.d() ---------------------------------------------------------------

set.seed(1)
treatment <- rnorm(100, mean = 10)
control <- rnorm(100, mean = 12)
d <- c(treatment, control)
f <- rep(c("Treatment", "Control"), each = 100)

cohen_d <- cohen.d(d ~ f)
hedges_g <- cohen.d(d ~ f, hedges.correction = TRUE)

statistics <- statistics |>
  add_stats(cohen_d) |>
  add_stats(hedges_g)

cohen_d
hedges_g

# VD.A() ------------------------------------------------------------------

vda <- VD.A(d ~ f)

statistics <- add_stats(statistics, vda)

vda

# cliff.delta() -----------------------------------------------------------

treatment <- c(10, 10, 20, 20, 20, 30, 30, 30, 40, 50)
control <- c(10, 20, 30, 40, 40, 50)

cliffs_delta <- cliff.delta(treatment, control, return.dm = TRUE)

statistics <- add_stats(statistics, cliffs_delta)

cliffs_delta

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/effsize.json")

# Cleanup -----------------------------------------------------------------

rm(
  statistics, control, d, f, treatment, cohen_d, hedges_g, vda, cliffs_delta,
  df
)
