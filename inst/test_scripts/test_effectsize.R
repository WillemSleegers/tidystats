
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(effectsize)

# Load data
data(mtcars)
mtcars$am <- factor(mtcars$am)
data(sleep)

# Create an empty list
results <- list()

# cohens_d() --------------------------------------------------------------

# Run analysis
cohens_d <- cohens_d(mpg ~ am, data = mtcars)
cohens_d_not_pooled <- cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
cohens_d_mu <- cohens_d(mpg ~ am, data = mtcars, mu = -5)
cohens_d_less <- cohens_d(mpg ~ am, data = mtcars, alternative = "less")
cohens_d_one_sample <- cohens_d(wt ~ 1, data = mtcars)
cohens_d_paired <- cohens_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, 
  data = sleep)

cohens_d
cohens_d_not_pooled
cohens_d_mu
cohens_d_less
cohens_d_one_sample
cohens_d_paired

# Tidy stats
temp <- tidy_stats(d1)

# Add stats
results <- results %>%
  add_stats(d1)

# hedges_g() --------------------------------------------------------------

hedges_g <- hedges_g(mpg ~ am, data = mtcars)
hedges_g

# glass_delta() -----------------------------------------------------------

glass_delta <- glass_delta(mpg ~ am, data = mtcars)
glass_delta$Glass_delta


