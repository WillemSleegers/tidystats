
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
statistics <- list()

# glm() -------------------------------------------------------------------

# Get data
# Example 1: Dobson (1990) Page 93: Randomized Controlled Trial
d.AD <- tibble(
  treatment = gl(3, 3),
  outcome = gl(3, 1, 9),
  counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12)
)

# Example 2: Venables & Ripley (2002, p.189)
anorexia <- tibble(
  Treat = c("Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", 
    "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", 
    "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", 
    "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", 
    "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", 
    "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "FT", 
    "FT", "FT", "FT", "FT", "FT", "FT", "FT", "FT", "FT", "FT", "FT", "FT", 
    "FT", "FT", "FT", "FT"),
  Prewt = c(80.7, 89.4, 91.8, 74.0, 78.1, 88.3, 87.3, 75.1, 80.6, 78.4, 77.6, 
    88.7, 81.3, 78.1, 70.5, 77.3, 85.2, 86.0, 84.1, 79.7, 85.5, 84.4, 79.6, 
    77.5, 72.3, 89.0, 80.5, 84.9, 81.5, 82.6, 79.9, 88.7, 94.9, 76.3, 81.0, 
    80.5, 85.0, 89.2, 81.3, 76.5, 70.0, 80.4, 83.3, 83.0, 87.7, 84.2, 86.4, 
    76.5, 80.2, 87.8, 83.3, 79.7, 84.5, 80.8, 87.4, 83.8, 83.3, 86.0, 82.5, 
    86.7, 79.6, 76.9, 94.2, 73.4, 80.5, 81.6, 82.1, 77.6, 83.5, 89.9, 86.0, 
    87.3),
  Postwt = c( 80.2, 80.1, 86.4, 86.3, 76.1, 78.1, 75.1, 86.7, 73.5, 84.6, 77.4, 
    79.5, 89.6, 81.4, 81.8, 77.3, 84.2, 75.4, 79.5, 73.0, 88.3, 84.7, 81.4, 
    81.2, 88.2, 78.8, 82.2, 85.6, 81.4, 81.9, 76.4, 103.6, 98.4, 93.4, 73.4, 
    82.1, 96.7, 95.3, 82.4, 72.5, 90.9, 71.3, 85.4, 81.6, 89.1, 83.9, 82.7, 
    75.7, 82.6, 100.4, 85.2, 83.6, 84.6, 96.2, 86.7, 95.2, 94.3, 91.5, 91.9, 
    100.3, 76.7, 76.8, 101.6, 94.9, 75.2, 77.8, 95.5, 90.7, 92.5, 93.8, 91.7, 
    98.0)
)

# Example 3: McCullagh & Nelder (1989, pp. 300-2)
clotting <- tibble(
  u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
  lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
  lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)
)

# Example 4: Logistic regression
admission <- tibble(
  admit = c(0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0),
  gre = c(380, 660, 800, 640, 520, 760, 560, 400, 540, 700, 800),
  gpa = c(3.61, 3.67, 4.00, 3.19, 2.93, 3.00, 2.98, 3.08, 3.39, 3.92, 4.00),
  rank = c(3, 3, 1, 4, 4, 2, 1, 2, 3, 2, 4)
)

# Run analyses
glm_poisson <- glm(counts ~ outcome + treatment, data = d.AD, 
  family = poisson())
glm_gaussian <- glm(Postwt ~ Prewt + Treat + offset(Prewt), data = anorexia)
glm_gamma <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
glm_gamma_fs <- glm(lot2 ~ log(u) + log(u^2), data = clotting, family = Gamma)
glm_binomial <- glm(admit ~ gre + gpa + rank, data = admission, 
  family = binomial(link = "logit"))

# Add stats
statistics <- statistics %>%
  add_stats(glm_poisson) %>%
  add_stats(glm_gaussian) %>%
  add_stats(glm_gamma) %>%
  add_stats(glm_gamma_fs) %>%
  add_stats(glm_binomial)

# Inspect output
summary(glm_poisson)
summary(glm_gaussian)
summary(glm_gamma)
summary(glm_gamma_fs)
summary(glm_binomial)

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/testthat/data/glm.json")

# Cleanup -----------------------------------------------------------------

rm(admission, anorexia, clotting, d.AD, glm_binomial, glm_gamma, glm_gamma_fs,
  glm_gaussian, glm_poisson, df, statistics)
