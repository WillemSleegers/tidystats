
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
results <- list()

# class: confint() --------------------------------------------------------

# Example 1
fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)

CI_fit <- confint(fit)
CI_fit

CI_fit_wt <- confint(fit, "wt")
CI_fit_wt

# Example 2
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9); treatment <- gl(3, 3)
glm_D93 <- glm(counts ~ outcome + treatment, family = poisson())

CI_glm_D93_MASS <- confint(glm_D93) # needs MASS to be installed
CI_glm_D93_MASS
CI_glm_D93_default <- confint.default(glm_D93)  # based on asymptotic normality
CI_glm_D93_default

# Tidy stats
temp <- tidy_stats(CI_fit)
temp <- tidy_stats(CI_fit_wt)
temp <- tidy_stats(CI_glm_D93_MASS)
temp <- tidy_stats(CI_glm_D93_default)

# Add stats
results <- list()

results <- results %>%
  add_stats(CI_fit, class = "confint") %>%
  add_stats(CI_fit_wt, class = "confint") %>%
  add_stats(CI_glm_D93_MASS, class = "confint") %>%
  add_stats(CI_glm_D93_default, class = "confint")

# Save stats
write_stats(results, "inst/test_data/confint.json")