# Setup -------------------------------------------------------------------

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

mod <- lm(mpg ~ hp * am * vs, data = mtcars)
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

# Linear model
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am + factor(cyl), tmp)
comp <- avg_comparisons(mod, variables = list(cyl = "reference"))

# Numeric contrasts
mod <- lm(mpg ~ hp, data = mtcars)
comp_num <- avg_comparisons(mod, variables = list(hp = 1))

# Custom difference
dat <- mtcars
dat$new_hp <- 49 * (dat$hp - min(dat$hp)) / (max(dat$hp) - min(dat$hp)) + 1
modlog <- lm(mpg ~ log(new_hp) + factor(cyl), data = dat)
fdiff <- \(x) data.frame(x, x + 10)
comp_custom <- avg_comparisons(modlog, variables = list(new_hp = fdiff))

# Adjusted Risk Ratio: see the contrasts vignette
mod <- glm(vs ~ mpg, data = mtcars, family = binomial)
comp_rr <- avg_comparisons(mod, comparison = "lnratioavg", transform = exp)

# Cross contrasts
mod <- lm(mpg ~ factor(cyl) * factor(gear) + hp, data = mtcars)
comp_cross <- avg_comparisons(mod, variables = c("cyl", "gear"), cross = TRUE)

# Variable-specific contrasts
comp_var <- avg_comparisons(mod, variables = list(gear = "sequential", hp = 10))

# Hypothesis test
mod <- lm(mpg ~ wt + drat, data = mtcars)
comp_hypothesis <- comparisons(
  mod,
  newdata = "mean",
  hypothesis = "wt = drat"
)

# Two custom contrasts using a matrix of weights
lc <- matrix(
  c(
    1, -1,
    2, 3
  ),
  ncol = 2
)
comp_matrix <- comparisons(
  mod,
  newdata = "mean",
  hypothesis = lc
)

# `by` argument
mod <- lm(mpg ~ hp * am * vs, data = mtcars)
comp_by <- comparisons(mod, by = TRUE)

mod <- lm(mpg ~ hp * am * vs, data = mtcars)
comp_by_2 <- avg_comparisons(mod, variables = "hp", by = c("vs", "am"))

# multinom
mod <- multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)
by <- data.frame(
  group = c("3", "4", "5"),
  by = c("3,4", "3,4", "5")
)
comp_multinom <- comparisons(mod, type = "probs", by = by)

statistics <- statistics |>
  add_stats(comp) |>
  add_stats(comp_num) |>
  add_stats(comp_custom) |>
  add_stats(comp_rr) |>
  add_stats(comp_cross) |>
  add_stats(comp_var) |>
  add_stats(comp_hypothesis) |>
  add_stats(comp_matrix) |>
  add_stats(comp_by) |>
  add_stats(comp_by_2) |>
  add_stats(comp_multinom)

comp
comp_num
comp_custom
comp_rr
comp_cross
comp_var
comp_hypothesis
comp_matrix
comp_by
comp_by_2
comp_multinom

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
