
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)

# Create an empty list
statistics <- list()

# confint() ---------------------------------------------------------------

# Get data
D93 <- tibble(
  counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
  outcome = gl(3, 1, 9),
  treatment = gl(3, 3)  
)

# Run analyses
fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
CI_fit <- confint(fit)
CI_fit_wt <- confint(fit, "wt")

glm_D93 <- glm(counts ~ outcome + treatment, data = D93, family = poisson())
CI_glm_D93_MASS <- confint(glm_D93) # based on profile likelihood
CI_glm_D93_default <- confint.default(glm_D93) # based on asymptotic normality

# Add stats
results <- results %>%
  add_stats(CI_fit, class = "confint") %>%
  add_stats(CI_fit_wt, class = "confint") %>%
  add_stats(CI_glm_D93_MASS, class = "confint") %>%
  add_stats(CI_glm_D93_default, class = "confint")

# Inspect output
CI_fit
CI_fit_wt
CI_glm_D93_MASS
CI_glm_D93_default

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_test_stats(results, "tests/data/confint.json")

# Cleanup -----------------------------------------------------------------

rm(
  CI_fit, CI_fit_wt, CI_glm_D93_MASS, CI_glm_D93_default, D93, fit, glm_D93,
  df, results
)
