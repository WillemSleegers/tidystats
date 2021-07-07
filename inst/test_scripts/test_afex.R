
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(afex)

# Create an empty list
results <- list()

# Analysis: aov_ez -------------------------------------------------------

# Load data
data("fhch2010", package = "afex")
fhch <- fhch2010[ fhch2010$correct,]

# Run analysis
aov_ez <- aov_ez("id", "log_rt", fhch, between = "task", 
  within = c("stimulus", "length"))

# Tidy stats
temp <- tidy_stats(aov_ez)

# Add stats
results <- results %>%
  add_stats(aov_ez)

# Issue: #11 --------------------------------------------------------------

library(tidyverse)
library(afex)
library(tidystats)
library(emmeans)

participant <- rep(seq(1, 10), each = 21)
condition <- rep(c("cond_1", "cond_2", "cond_3"), times = 70)
time <- rep(seq(0, 30, 5), each = 3, times = 10)
response <- rnorm(n = 210, mean = c(1.1, 1.2, 1.3), sd = 0.1)

df <- data.frame(participant = factor(participant),
                 time = factor(time),
                 condition = factor(condition),
                 response)

model <- mixed(response ~ condition * time +
                 (condition | participant),
               df)

emm_results <- emmeans(model, "condition", by = "time") %>% 
  contrast("trt.vs.ctrl") %>%
  test(by = NULL)

results <- list()

x <- tidy_stats.summary_emm(emm_results)

results <- results %>% 
  add_stats(emm_results)

# Write stats
write_stats(results, "inst/test_data/afex.json")
