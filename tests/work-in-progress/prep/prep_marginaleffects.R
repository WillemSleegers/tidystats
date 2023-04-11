# Setup -------------------------------------------------------------------

library(dplyr)
library(marginaleffects)
library(nnet)

statistics <- list()

# avg_predictions() ------------------------------------------------------

mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
pred <- avg_predictions(mod)
pred_grid <- avg_predictions(
  mod,
  newdata = datagrid(hp = c(100, 120), cyl = 4)
)

mod <- lm(mpg ~ hp * am * vs, mtcars)
pred_by <- avg_predictions(mod, by = "am")
pred_by_two <- avg_predictions(mod, by = c("am", "vs"))
pred_vars <- avg_predictions(mod, variables = list(hp = c(90, 110)))

mod <- lm(mpg ~ wt + drat, data = mtcars)
pred_hypothesis <- predictions(
  mod,
  newdata = datagrid(wt = 2:3),
  hypothesis = "b1 = b2"
)
pred_hypothesis_two <- predictions(
  mod,
  newdata = datagrid(wt = 2:3),
  hypothesis = matrix(c(1, -1, 2, 3), ncol = 2)
)

nom <- multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)
pred_nom <- avg_predictions(nom, type = "probs", by = "group")

mod <- multinom(factor(cyl) ~ mpg + am, data = mtcars, trace = FALSE)
pred_nom_by <- predictions(
  mod,
  newdata = "mean",
  byfun = sum,
  by = data.frame(
    by = c("4,6", "4,6", "8"),
    group = as.character(c(4, 6, 8))
  )
)

statistics <- statistics |>
  add_stats(pred) |>
  add_stats(pred_grid) |>
  add_stats(pred_by) |>
  add_stats(pred_by_two) |>
  add_stats(pred_vars) |>
  add_stats(pred_hypothesis) |>
  add_stats(pred_hypothesis_two) |>
  add_stats(pred_nom) |>
  add_stats(pred_nom_by)

pred
pred_grid
pred_by
pred_by_two
pred_vars
pred_hypothesis
pred_hypothesis_two
pred_nom
pred_nom_by

# avg_comparisons() -------------------------------------------------------

tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am + factor(cyl), tmp)
comp <- avg_comparisons(mod, variables = list(cyl = "reference"))

comp

statistics <- statistics |>
  add_stats(comp)

mod <- lm(mpg ~ hp, data = mtcars)
comp_one <- avg_comparisons(mod, variables = list(hp = 1))
comp_one

statistics <- statistics |>
  add_stats(comp_one)

mod <- lm(mpg ~ factor(cyl) * factor(gear) + hp, data = mtcars)
comp_cross <- avg_comparisons(mod, variables = c("cyl", "gear"), cross = TRUE)
comp_cross

statistics <- statistics |>
  add_stats(comp_cross)

# variable-specific contrasts
avg_comparisons(mod, variables = list(gear = "sequential", hp = 10))

# hypothesis test: is the `hp` marginal effect at the mean equal to the `drat` marginal effect
mod <- lm(mpg ~ wt + drat, data = mtcars)

# same hypothesis test using row indices
avg_comparisons(
  mod,
  newdata = "mean",
  hypothesis = "b1 - b2 = 0"
)

# two custom contrasts using a matrix of weights
lc <- matrix(
  c(
    1, -1,
    2, 3
  ),
  ncol = 2
)
avg_comparisons(
  mod,
  newdata = "mean",
  hypothesis = lc
)

# `by` argument
mod <- lm(mpg ~ hp * am * vs, data = mtcars)
avg_comparisons(mod, by = TRUE)
avg_comparisons(mod, variables = "hp", by = c("vs", "am"))

library(nnet)
mod <- multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)
by <- data.frame(
  group = c("3", "4", "5"),
  by = c("3,4", "3,4", "5")
)
avg_comparisons(mod, type = "probs", by = by)

# avg_slopes() ------------------------------------------------------------

mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
mfx <- slopes(mod)

summary(mfx)
avg_slopes(mod)
slopes(mod, newdata = datagrid())
x <- slopes(mod, newdata = datagrid(hp = c(100, 110)))



mod2 <- lm(mpg ~ hp * cyl, data = mtcars)
avg_slopes(mod2, variables = "hp", by = "cyl")


slopes(mod,
  newdata = datagrid(
    hp = c(100, 110),
    grid_type = "counterfactual"
  )
) |>
  summary()


slopes(mod, vcov = sandwich::vcovHC(mod)) |>
  summary()

mod <- lm(mpg ~ wt + drat, data = mtcars)
slopes(
  mod,
  newdata = "mean",
  hypothesis = "wt = drat"
) |> summary()
slopes(
  mod,
  newdata = "mean",
  hypothesis = "b1 - b2 = 0"
)
slopes(
  mod,
  newdata = "mean",
  hypothesis = c(1, -1)
)

lc <- matrix(
  c(
    1, -1,
    2, 3
  ),
  ncol = 2
)
colnames(lc) <- c("Contrast A", "Contrast B")
slopes(
  mod,
  newdata = "mean",
  hypothesis = lc
)

# Add stats
results <- results %>%
  add_stats(marginaleffects_summary)

# Inspect output
marginaleffects_summary

# marginal_means() --------------------------------------------------------

# hypotheses() ------------------------------------------------------------

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/marginaleffects.json")

# Cleanup -----------------------------------------------------------------
