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
