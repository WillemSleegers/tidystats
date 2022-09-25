
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(emmeans)

# Create an empty list
statistics <- list()

# emmeans() --------------------------------------------------------------------

# Run analyses
model <- lm(breaks ~ wool, data = warpbreaks)
emm_spec <- emmeans(model, specs = ~ wool)

warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
emm_specs <- emmeans(warp_lm,  ~ wool + tension)
emm_spec_by <- emmeans(warp_lm,  ~ wool | tension)

iris_lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = iris)
emmeans(iris_lm, ~ Sepal.Width | Petal.Length + Species)

emm_spec_by_adjust <- emmeans(warp_lm, poly ~ tension | wool, adjust = "sidak")

# Add stats
statistics <- statistics %>%
  add_stats(emm_spec) 
  add_stats(emm_specs) %>%
  add_stats(emm_spec_by) %>%
  add_stats(emm_spec_by_adjust)

# Inspect output
emm_spec
emm_specs
emm_spec_by
emm_spec_by_adjust

# test() ------------------------------------------------------------------

# Run analysis
pigs_lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
emmeans_response <- emmeans(pigs_lm, specs = "percent", type = "response")

emmeans_test <- test(
  emmeans_response, 
  null = log(35), 
  delta = log(1.10), 
  side = ">"
)
emmeans_test_joint <- test(emmeans_response, joint = TRUE)

# Add stats
statistics <- statistics %>%
  add_stats(emmeans_response) %>%
  add_stats(emmeans_test) %>%
  add_stats(emmeans_test_joint)

# Inspect output
emmeans_response
emmeans_test
emmeans_test_joint

# contrast() -------------------------------------------------------------

# Run analyses
warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
warp_emm <- emmeans(warp_lm, ~ tension | wool)

warp_contrast_poly <- contrast(warp_emm, "poly")
warp_constrast_simple <- contrast(warp_emm, simple = c("wool", "tension"))
warp_contrast_list <- contrast(warp_emm, simple = list("wool", "tension"))

tw_emm <- contrast(
  warp_emm, 
  interaction = c(tension = "poly", wool = "consec"), 
  by = NULL
)

iris_lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
emmeans(iris_lm, specs = c("Sepal.Width", "Petal.Length"))
iris_contrast <- contrast(
  warp_emm, 
  interaction = c(tension = "poly", wool = "consec"), 
  by = NULL
)

mod <- lm(Water.Temp ~ poly(stack.loss, degree = 2), data = stackloss)
emm <- emmeans(mod, "stack.loss", at = list(stack.loss = 10 * (1:4)))
emm

# Convert results from Celsius to Fahrenheit:
confint(contrast(emm, "identity", scale = 9/5, offset = 32))

emmeans_contrast <- contrast(pigs_emm, "consec")

# Add stats
statistics <- statistics %>%
  add_stats(tw_emm)

# Inspect output
emmeans_contrast
tw_emm

# mvcontrast() ------------------------------------------------------------

# Get data
MOats.lm <- lm(yield ~ Variety + Block, data = MOats)
MOats.emm <- emmeans(MOats.lm, ~ Variety | rep.meas)

# Run analysis
emmeans_mvcontrast <- mvcontrast(MOats.emm, "consec", show.ests = TRUE)

# Test each mean against a specified null vector
emmeans_mvcontrast_named <- mvcontrast(
  MOats.emm, 
  "identity", 
  name = "Variety", 
  null = c(80, 100, 120, 140)
)

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

# emtrends() --------------------------------------------------------------

# Run analysis
fiber.lm <- lm(strength ~ diameter*machine, data=fiber)
# Suppose we want trends relative to sqrt(diameter)...
emtrends_basic = emtrends(fiber.lm, ~ machine | diameter, 
  var = "sqrt(diameter)", at = list(diameter = c(20, 30)))

# Obtaining a reference grid
mtcars.lm <- lm(mpg ~ poly(disp, degree = 2) * (factor(cyl) + factor(am)), 
  data = mtcars)

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

# joint_tests() -----------------------------------------------------------

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

# ref_grid() --------------------------------------------------------------

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

write_test_stats(results, "tests/testthat/data/emmeans.json")

# Cleanup -----------------------------------------------------------------


