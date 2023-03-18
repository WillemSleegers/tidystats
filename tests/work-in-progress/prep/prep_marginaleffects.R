# Setup -------------------------------------------------------------------

library(marginaleffects)

statistics <- list()

# slopes() ----------------------------------------------------------------

mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
mfx <- slopes(mod)

summary(mfx)
avg_slopes(mod, by = TRUE)
slopes(mod, newdata = datagrid())
slopes(mod, newdata = datagrid(hp = c(100, 110)))

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

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_test_stats(results, "tests/data/marginaleffects.json")

# Cleanup -----------------------------------------------------------------
