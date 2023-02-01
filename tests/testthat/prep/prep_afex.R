
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(afex)

# Create an empty list
statistics <- list()

# aov_ez() ----------------------------------------------------------------

# Get data
data(md_12.1)

# Run analyses
aov_ez <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), 
  anova_table = list(correction = "none", es = "none"))
aov_ez_default <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))

# Add stats
statistics <- statistics %>%
  add_stats(aov_ez) %>%
  add_stats(aov_ez_default)

# Inspect output 
summary(aov_ez)
summary(aov_ez_default)

# aov_car() ---------------------------------------------------------------

# Get data
data(obk.long, package = "afex")

# Run analyses
aov_car <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)), 
  data = obk.long, observed = "gender")
aov_car_covariate <- aov_car(value ~ treatment * gender + age + 
    Error(id/(phase*hour)), data = obk.long, observed = c("gender", "age"), 
  factorize = FALSE)
aov_car_aggregate <- aov_car(value ~ treatment * gender + Error(id/hour), 
  data = obk.long, observed = "gender")
aov_car_aggregate_both <- aov_car(value ~ treatment * gender + Error(id), 
  data = obk.long, observed = c("gender"))
aov_car_within <- aov_car(value ~ Error(id/(phase*hour)), data = obk.long)
aov_car_no_df_pes <- aov_car(value ~ treatment * gender + 
    Error(id/(phase*hour)), data = obk.long, 
  anova_table = list(correction = "none", es = "pes"))
aov_car_no_df_no_MSE <- aov_car(value ~ treatment * gender + 
    Error(id/(phase*hour)), data = obk.long,observed = "gender", 
  anova_table = list(correction = "none", MSE = FALSE))

# Add stats
statistics <- statistics %>%
  add_stats(aov_car) %>%
  add_stats(aov_car_covariate) %>%
  add_stats(aov_car_aggregate_both) %>%
  add_stats(aov_car_within) %>%
  add_stats(aov_car_no_df_pes) %>%
  add_stats(aov_car_no_df_no_MSE)

# Inspect output
summary(aov_car)
summar(aov_car_covariate)
summary(aov_car_aggregate)
summary(aov_car_aggregate_both)
summary(aov_car_within)
summary(aov_car_no_df_pes)
summary(aov_car_no_df_no_MSE)

# Add stats

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
