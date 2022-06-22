
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(emmeans)

# Create an empty list
results <- list()

# emmeans() --------------------------------------------------------------------

# Run analysis
emmeans_single <- emmeans(lm(breaks ~ wool, data = warpbreaks),  ~ wool)

warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
emmeans_multi <- emmeans(warp.lm,  ~ wool | tension)

# with 'adjust' argument
emmeans_adjust <- emmeans(warp.lm, poly ~ tension | wool, adjust = "sidak")

# Add stats
results = results %>%
  add_stats(emmeans_single) %>%
  add_stats(emmeans_multi) %>%
  add_stats(emmeans_adjust)

# Inspect output
emmeans_single
emmeans_multi
emmeans_adjust$emmeans
emmeans_adjust$contrasts

# summary(emmeans()) --------------------------------------------------------------------

# Run analysis
emmeans_summary_single <- summary(emmeans(lm(breaks ~ wool, data = warpbreaks),  ~ wool))

warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
emmeans_summary_multi <- summary(emmeans(warp.lm,  ~ wool | tension))

# with 'adjust' argument
emmeans_summary_adjust <- summary(emmeans(warp.lm, poly ~ tension | wool, adjust = "sidak"))

# Add stats
results = results %>%
  add_stats(emmeans_summary_single) %>%
  add_stats(emmeans_summary_multi) %>%
  add_stats(emmeans_summary_adjust)

# Inspect output
emmeans_summary_single
emmeans_summary_multi
emmeans_summary_adjust$emmeans
emmeans_summary_adjust$contrasts


# confint(emmeans()) --------------------------------------------------------------------

# Run analysis

warp.emm <- emmeans(warp.lm, ~ tension | wool)

emmeans_confint <- confint(warp.emm, level = .85)
emmeans_confint_single <- confint(warp.emm, by = NULL, level = .90)

# Add stats
results = results %>%
  add_stats(emmeans_confint) %>%
  add_stats(emmeans_confint_single)

# Inspect output
emmeans_confint
emmeans_confint_single


# test(emmeans()) --------------------------------------------------------------------

# Get data
pigs.lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
pigs.emm <- emmeans(pigs.lm, "percent", type = "response")

# Run analysis
# For which percents is EMM non-inferior to 35, based on a 10% threshold?
emmeans_test <- test(pigs.emm, null = log(35), delta = log(1.10), side = ">")
emmeans_test_joint <- test(pigs.emm, joint = TRUE)

# Add stats
results = results %>%
  add_stats(emmeans_test) %>%
  add_stats(emmeans_test_joint)

# Inspect output
emmeans_test
emmeans_test_joint

# contrast() --------------------------------------------------------------------
# Get data
set.seed(1234)
pigs.lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
pigs.emm <- emmeans(pigs.lm, "percent", type = "response")

# Run analysis
emmeans_contrast <- contrast(pigs.emm, "consec")

# Add stats
results = results %>%
  add_stats(emmeans_contrast)

# Inspect output
emmeans_contrast


# mvcontrast() --------------------------------------------------------------------

# Get data
MOats.lm <- lm(yield ~ Variety + Block, data = MOats)
MOats.emm <- emmeans(MOats.lm, ~ Variety | rep.meas)

# Run analysis
emmeans_mvcontrast <- mvcontrast(MOats.emm, "consec", show.ests = TRUE)

# Test each mean against a specified null vector
emmeans_mvcontrast_named <- mvcontrast(MOats.emm, "identity", name = "Variety", null = c(80, 100, 120, 140))

# Add stats
results = results %>%
  add_stats(emmeans_mvcontrast) %>%
  add_stats(emmeans_mvcontrast_named)

# Inspect output
emmeans_mvcontrast
emmeans_mvcontrast_named

# eff_size() --------------------------------------------------------------------

# Run analysis
fiber.lm <- lm(strength ~ diameter + machine, data = fiber)
emm <- emmeans(fiber.lm, "machine")
emmeans_eff_size <- eff_size(emm, sigma = sigma(fiber.lm), edf = df.residual(fiber.lm))

# Add stats
results = results %>%
  add_stats(emmeans_eff_size)

# Inspect output
emmeans_eff_size


# emtrends() --------------------------------------------------------------------

# Run analysis
fiber.lm <- lm(strength ~ diameter*machine, data=fiber)
# Suppose we want trends relative to sqrt(diameter)...
emtrends_basic = emtrends(fiber.lm, ~ machine | diameter, var = "sqrt(diameter)", at = list(diameter = c(20, 30)))

# Obtaining a reference grid
mtcars.lm <- lm(mpg ~ poly(disp, degree = 2) * (factor(cyl) + factor(am)), data = mtcars)

# Center trends at mean disp for each no. of cylinders
emtrends_cov_reduce <- emtrends(mtcars.lm, var = "disp", 
                          cov.reduce = disp ~ factor(cyl))

# Add stats
results = results %>%
  add_stats(emtrends_basic) %>%
  add_stats(emtrends_cov_reduce)

# Inspect output
emtrends_basic
emtrends_cov_reduce

# joint_tests() --------------------------------------------------------------------

# Get data
pigs.lm <- lm(log(conc) ~ source * factor(percent), data = pigs)

# Run analysis
joint_tests_single <- joint_tests(pigs.lm)
## separate joint tests of 'percent'
joint_tests_multi <- joint_tests(pigs.lm, by = "source")

# Add stats
results = results %>%
  add_stats(joint_tests_single) %>%
  add_stats(joint_tests_multi)

# Inspect output
joint_tests_single
joint_tests_multi


# ref_grid() --------------------------------------------------------------------

# Get data
fiber.lm <- lm(strength ~ machine*diameter, data = fiber)

# Run analysis
ref_grid_results = ref_grid(fiber.lm)

# Add stats
results = results %>%
  add_stats(ref_grid_results)

# Inspect output
ref_grid_results
summary(ref_grid_results)

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/emmeans.json")
