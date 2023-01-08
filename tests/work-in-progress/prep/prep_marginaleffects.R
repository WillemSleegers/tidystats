
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
mfx

# Average Marginal Effect (AME)
marginal <- marginaleffects(mod)

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
  type = "response")
)

# Adjusted Risk Ratio (see Case Study vignette on the website)
mod <- glm(vs ~ mpg, data = mtcars, family = binomial)
cmp <- comparisons(mod, transform_pre = "lnratioavg")
marginaleffects_comparisons_summary_arr <- summary(cmp, transform_post = exp)

# Add stats
results = results %>%
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