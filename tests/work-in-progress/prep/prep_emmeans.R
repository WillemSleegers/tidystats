
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(emmeans)

# Create an empty list
statistics <- list()

# emmeans() ---------------------------------------------------------------

# Run analyses
warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)

warp_emm <- emmeans(warp_lm, ~ tension | wool)
warp_emm_poly <- emmeans(warp_lm, poly ~ tension | wool, adjust = "sidak")
warp_emm_adjust <- emmeans(warp_lm, ~ tension | wool, adjust = "sidak")

warp_emm_confint <- confint(warp_emm, by = "wool", level = .90)

# Add stats
statistics <- statistics %>%
  add_stats(warp_emm) %>%
  add_stats(warp_emm_poly) %>%
  add_stats(warp_emm_adjust) %>%
  add_stats(warp_emm_confint)

# Inspect output
warp_emm
warp_emm_poly
warp_emm_adjust
warp_emm_confint

# contrast() -------------------------------------------------------------

# Run analyses
warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
warp_emm <- emmeans(warp_lm, ~ tension | wool)

warp_contrast_poly <- contrast(warp_emm, "poly")
warp_pairs <- pairs(warp_emm)

warp_contrast_eff <- contrast(
  warp_emm, "eff",
  by = NULL, enhance.levels = c("wool", "tension")
)
warp_pairs_simple <- pairs(warp_emm, simple = "wool")
warp_pairs_each <- pairs(warp_emm, simple = "each", combine = FALSE)
warp_pairs_each_combined <- pairs(warp_emm, simple = "each", combine = TRUE)

tw_emm <- contrast(
  warp_emm,
  interaction = c(tension = "poly", wool = "consec"),
  by = NULL
)

# Add stats
statistics <- statistics %>%
  add_stats(warp_contrast_poly) %>%
  add_stats(warp_pairs) %>%
  add_stats(warp_contrast_eff) %>%
  add_stats(warp_pairs_simple) %>%
  add_stats(warp_pairs_each) %>%
  add_stats(warp_pairs_each_combined) %>%
  add_stats(tw_emm)

# Inspect output
warp_contrast_poly
warp_pairs
warp_contrast_eff
warp_pairs_simple
warp_pairs_each
warp_pairs_each_combined
tw_emm

# test() ------------------------------------------------------------------

# Run analyses
pigs_lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
pigs_emm <- emmeans(pigs_lm, specs = "percent", type = "response")
pigs_con <- contrast(pigs_emm, "consec")

pigs_test <- test(
  pigs_emm,
  null = log(35),
  delta = log(1.10),
  side = ">"
)
pigs_test_joint <- test(pigs_con, joint = TRUE)

# Add stats
statistics <- statistics %>%
  add_stats(pigs_test) %>%
  add_stats(pigs_test_joint)

# Inspect output
pigs_test
pigs_test_joint

# mvcontrast() ------------------------------------------------------------

# Get data
moats_lm <- lm(yield ~ Variety + Block, data = MOats)
moats_emm <- emmeans(moats_lm, ~ Variety | rep.meas)

# Run analyses
moats_mvcontrast <- mvcontrast(moats_emm, "consec", show.ests = TRUE)
moats_mvcontrast_null <- mvcontrast(
  moats_emm,
  "identity",
  name = "Variety",
  null = c(80, 100, 120, 140)
)

# Add stats
statistics <- statistics %>%
  add_stats(moats_mvcontrast, class = "emm_list") %>%
  add_stats(moats_mvcontrast_null)

# Inspect output
moats_mvcontrast
moats_mvcontrast_null

# eff_size() --------------------------------------------------------------

# Run analysis
fiber_lm <- lm(strength ~ diameter + machine, data = fiber)
emm <- emmeans(fiber_lm, "machine")
emmeans_eff_size <- eff_size(
  emm,
  sigma = sigma(fiber.lm), edf = df.residual(fiber.lm)
)

# Add stats
results <- results %>%
  add_stats(emmeans_eff_size)

# Inspect output
emmeans_eff_size

# emtrends() --------------------------------------------------------------

# Run analysis
fiber_lm <- lm(strength ~ diameter * machine, data = fiber)
# Suppose we want trends relative to sqrt(diameter)...
emtrends_basic <- emtrends(fiber_lm, ~ machine | diameter,
  var = "sqrt(diameter)", at = list(diameter = c(20, 30))
)

# Obtaining a reference grid
mtcars_lm <- lm(mpg ~ poly(disp, degree = 2) * (factor(cyl) + factor(am)),
  data = mtcars
)

# Center trends at mean disp for each no. of cylinders
emtrends_cov_reduce <- emtrends(mtcars_lm,
  var = "disp",
  cov.reduce = disp ~ factor(cyl)
)

# Add stats
results <- results %>%
  add_stats(emtrends_basic) %>%
  add_stats(emtrends_cov_reduce)

# Inspect output
emtrends_basic
emtrends_cov_reduce

# joint_tests() -----------------------------------------------------------

# Get data
pigs_lm <- lm(log(conc) ~ source * factor(percent), data = pigs)

# Run analysis
joint_tests_single <- joint_tests(pigs_lm)
## separate joint tests of 'percent'
joint_tests_multi <- joint_tests(pigs_lm, by = "source")

# Add stats
results <- results %>%
  add_stats(joint_tests_single) %>%
  add_stats(joint_tests_multi)

# Inspect output
joint_tests_single
joint_tests_multi

# ref_grid() --------------------------------------------------------------

# Get data
fiber_lm <- lm(strength ~ machine * diameter, data = fiber)

# Run analysis
ref_grid_results <- ref_grid(fiber_lm)

# Add stats
results <- results %>%
  add_stats(ref_grid_results)

# Inspect output
ref_grid_results
summary(ref_grid_results)

# GitHub issue #8 ---------------------------------------------------------

library(afex)
library(tidystats)
library(emmeans)

participant <- rep(seq(1, 10), each = 21)
condition <- rep(c("cond_1", "cond_2", "cond_3"), times = 70)
time <- rep(seq(0, 30, 5), each = 3, times = 10)
response <- rnorm(n = 210, mean = c(1.1, 1.2, 1.3), sd = 0.1)

df <- data.frame(
  participant = factor(participant),
  time = factor(time),
  condition = factor(condition),
  response
)

model <- mixed(
  response ~ condition * time +
    (condition | participant),
  df
)

emm_results <- emmeans(model, "condition", by = "time") %>%
  contrast("pairwise")

results <- list()

results <- results %>%
  add_stats(emm_results)

write_stats(results, "test_results.json")




noise_lm <- lm(noise / 10 ~ size * type * side, data = auto.noise)
anova(noise.lm)
emmeans(noise.lm, pairwise ~ size)
emmeans(noise.lm, ~ size * side * type)

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/testthat/data/emmeans.json")

# Cleanup -----------------------------------------------------------------
