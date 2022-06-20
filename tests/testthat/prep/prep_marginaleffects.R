
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(marginaleffects)

# Create an empty list
results <- list()

# summary(marginaleffects()) --------------------------------------------------------------------

# Get data
mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)

# Average Marginal Effect (AME)
marginaleffects_summary <- summary(marginaleffects(mod))

# Add stats
results = results %>%
  add_stats(marginaleffects_summary)

# Inspect output
marginaleffects_summary


# summary(marginalmeans()) --------------------------------------------------------------------

# Get data
dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- lm(mpg ~ hp + cyl + am, data = dat)

# Compute and summarize marginal means
marginalmeans_summary <- summary(marginalmeans(mod))

# Add stats
results = results %>%
  add_stats(marginalmeans_summary)

# Inspect output
marginalmeans_summary


# summary(comparisons()) --------------------------------------------------------------------

# Linear model
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am + factor(cyl), tmp)
marginaleffects_comparisons_summary <- summary(comparisons(mod, 
  contrast_factor = "reference", type = "response"))

# Adjusted Risk Ratio (see Case Study vignette on the website)
mod <- glm(vs ~ mpg, data = mtcars, family = binomial)
cmp <- comparisons(mod, transform_pre = "lnratioavg")
marginaleffects_comparisons_summary_arr <- summary(cmp, transform_post = exp)

# Add stats
results = results %>%
  add_stats(marginaleffects_comparisons_summary) %>%
  add_stats(marginaleffects_comparisons_summary_arr)

# Inspect output
comparisons_summary
comparisons_summary_arr


# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/marginaleffects.json")
