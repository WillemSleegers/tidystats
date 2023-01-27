
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(marginaleffects)

# Create an empty list
statistics <- list()

# marginaleffects() -------------------------------------------------------

# Run analysis
mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
mfx <- marginaleffects(mod)

head(mfx)
class(mfx)

# Marginal Effect at the Mean (MEM)
marginaleffects(mod, newdata = datagrid())

# Marginal Effect at User-Specified Values
# Variables not explicitly included in `datagrid()` are held at their means
marginaleffects(mod, newdata = datagrid(hp = c(100, 110)))

# Heteroskedasticity robust standard errors
marginaleffects(mod, vcov = sandwich::vcovHC(mod))

# hypothesis test: is the `hp` marginal effect at the mean equal to the `drat`
# marginal effect
mod <- lm(mpg ~ wt + drat, data = mtcars)
marginaleffects(
  mod,
  newdata = "mean",
  hypothesis = "wt = drat"
)

# same hypothesis test using row indices
marginaleffects(
  mod,
  newdata = "mean",
  hypothesis = "b1 - b2 = 0"
)
# same hypothesis test using numeric vector of weights
marginaleffects(
  mod,
  newdata = "mean",
  hypothesis = c(1, -1)
)
# two custom contrasts using a matrix of weights
lc <- matrix(
  c(
    1, -1,
    2, 3
  ),
  ncol = 2
)
colnames(lc) <- c("Contrast A", "Contrast B")
marginaleffects(
  mod,
  newdata = "mean",
  hypothesis = lc
)

# Add stats
results <- results %>%
  add_stats(marginaleffects_summary)

# Inspect output
marginaleffects_summary

# summary(marginalmeans()) ------------------------------------------------

# Get data
dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- lm(mpg ~ hp + cyl + am, data = dat)

# Compute and summarize marginal means
marginalmeans_summary <- summary(marginalmeans(mod))

# Add stats
results <- results %>%
  add_stats(marginalmeans_summary)

# Inspect output
marginalmeans_summary

# summary(comparisons()) --------------------------------------------------

# Linear model
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am + factor(cyl), tmp)
marginaleffects_comparisons <- comparisons(
  model = mod,
  contrast_factor = "reference",
  type = "response"
)


# Adjusted Risk Ratio (see Case Study vignette on the website)
mod <- glm(vs ~ mpg, data = mtcars, family = binomial)
cmp <- comparisons(mod, transform_pre = "lnratioavg")
marginaleffects_comparisons_summary_arr <- summary(cmp, transform_post = exp)

# Add stats
results <- results %>%
  add_stats(marginaleffects_comparisons_summary) %>%
  add_stats(marginaleffects_comparisons_summary_arr)

# Inspect output
marginaleffects_comparisons_summary
marginaleffects_comparisons_summary_arr

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_test_stats(results, "tests/data/marginaleffects.json")

# Cleanup -----------------------------------------------------------------
