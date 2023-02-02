
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

# Run analyses
fiber_lm <- lm(strength ~ diameter + machine, data = fiber)
fiber_emm <- emmeans(fiber_lm, "machine")
fiber_eff_size <- eff_size(
  fiber_emm,
  sigma = sigma(fiber_lm), edf = df.residual(fiber_lm)
)

moats_lm <- lm(yield ~ Variety, data = MOats)
moats_emm <- emmeans(moats_lm, "Variety")
moats_eff_size <- eff_size(
  moats_emm,
  sigma = sqrt(mean(sigma(moats_lm)^2)),
  edf = df.residual(moats_lm)
)

# Add stats
statistics <- statistics %>%
  add_stats(fiber_eff_size) %>%
  add_stats(moats_eff_size)

# Inspect output
fiber_eff_size
moats_eff_size

# emtrends() --------------------------------------------------------------

# Run analyses
fiber_lm <- lm(strength ~ diameter * machine, data = fiber)

fiber_trends <- emtrends(
  fiber_lm, ~ machine | diameter,
  var = "sqrt(diameter)",
  at = list(diameter = c(20, 30))
)

mtcars_lm <- lm(
  mpg ~ poly(disp, degree = 2) * (factor(cyl) + factor(am)),
  data = mtcars
)

mtcars_trends <- emtrends(
  mtcars_lm,
  var = "disp",
  cov.reduce = disp ~ factor(cyl)
)

# Add stats
statistics <- statistics %>%
  add_stats(fiber_trends) %>%
  add_stats(mtcars_trends)

# Inspect output
fiber_trends
mtcars_trends
summary(mtcars_trends)

# joint_tests() -----------------------------------------------------------

# Get data
pigs_lm <- lm(log(conc) ~ source * factor(percent), data = pigs)

# Run analysis
pigs_joint_tests <- joint_tests(pigs_lm)
pigs_joint_tests_by <- joint_tests(pigs_lm, by = "source")

# Add stats
statistics <- statistics %>%
  add_stats(pigs_joint_tests) %>%
  add_stats(pigs_joint_tests_by)

# Inspect output
pigs_joint_tests
pigs_joint_tests_by

# ref_grid() --------------------------------------------------------------

# Get data
fiber_lm <- lm(strength ~ machine * diameter, data = fiber)

# Run analysis
fiber_ref_grid <- ref_grid(fiber_lm)

# Add stats
statistics <- statistics %>%
  add_stats(fiber_ref_grid)

# Inspect output
fiber_ref_grid
summary(fiber_ref_grid)

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/testthat/data/emmeans.json")

# Cleanup -----------------------------------------------------------------

rm(
  warp_lm, warp_emm, warp_emm_poly, warp_emm_adjust, warp_emm_confint,
  warp_contrast_poly, warp_pairs, warp_contrast_eff, warp_pairs_simple,
  warp_pairs_each, warp_pairs_each_combined, tw_emm, moats_lm, moats_emm,
  moats_eff_size, moats_mvcontrast, moats_mvcontrast_null, fiber_lm, fiber_emm,
  fiber_eff_size, fiber_ref_grid, fiber_trends, mtcars_lm, mtcars_trends,
  pigs_lm, pigs_emm, pigs_con, pigs_test, pigs_test_joint, pigs_joint_tests,
  pigs_joint_tests_by, statistics, df
)
