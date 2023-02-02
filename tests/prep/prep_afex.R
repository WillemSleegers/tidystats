# Notes -------------------------------------------------------------------

# Could store more statistics by running `summary()` on the output of these
# functions.

# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(afex)

# Create an empty list
statistics <- list()

# aov_ez() ----------------------------------------------------------------

# Get data
data(md_12.1)
data(obk.long, package = "afex")

# Run analyses
aov_ez <- aov_ez(
  "id",
  "rt",
  md_12.1,
  within = c("angle", "noise"),
  anova_table = list(correction = "none", es = "none")
)
aov_ez_default <- aov_ez(
  "id",
  "rt",
  md_12.1,
  within = c("angle", "noise")
)
aov_ez_covariate <- aov_ez(
  "id",
  "value",
  obk.long,
  between = c("treatment", "gender"),
  within = c("phase", "hour"), covariate = "age",
  observed = c("gender", "age"),
  factorize = FALSE
)
aov_ez_aggregate <- aov_ez(
  "id",
  "value",
  obk.long,
  c("treatment", "gender"),
  "hour",
  observed = "gender"
)
aov_ez_aggregate_both <- aov_ez(
  "id",
  "value",
  obk.long,
  between = c("treatment", "gender"),
  observed = "gender"
)
aov_ez_p <- aov_ez(
  "id", "value",
  obk.long,
  between = "treatment",
  within = c("phase", "hour"),
  anova_table = list(p_adjust_method = "holm")
)

# Add stats
statistics <- statistics %>%
  add_stats(aov_ez) %>%
  add_stats(aov_ez_default) %>%
  add_stats(aov_ez_covariate) %>%
  add_stats(aov_ez_aggregate) %>%
  add_stats(aov_ez_aggregate_both) %>%
  add_stats(aov_ez_p)

# Inspect output
aov_ez
aov_ez_default
aov_ez_covariate
aov_ez_aggregate
aov_ez_aggregate_both
aov_ez_p

# aov_car() ---------------------------------------------------------------

# Run analyses
aov_car <- aov_car(
  value ~ treatment * gender + Error(id / (phase * hour)),
  data = obk.long, observed = "gender"
)
aov_car_covariate <- aov_car(
  value ~ treatment * gender + age + Error(id / (phase * hour)),
  data = obk.long, observed = c("gender", "age"),
  factorize = FALSE
)
aov_car_aggregate <- aov_car(
  value ~ treatment * gender + Error(id / hour),
  data = obk.long, observed = "gender"
)
aov_car_aggregate_both <- aov_car(
  value ~ treatment * gender + Error(id),
  data = obk.long, observed = "gender"
)
aov_car_within <- aov_car(
  value ~ Error(id / (phase * hour)),
  data = obk.long
)
aov_car_no_df_pes <- aov_car(
  value ~ treatment * gender + Error(id / (phase * hour)),
  data = obk.long,
  anova_table = list(correction = "none", es = "pes")
)
aov_car_no_df_no_MSE <- aov_car(
  value ~ treatment * gender + Error(id / (phase * hour)),
  data = obk.long, observed = "gender",
  anova_table = list(correction = "none", MSE = FALSE)
)

# Add stats
statistics <- statistics %>%
  add_stats(aov_car) %>%
  add_stats(aov_car_covariate) %>%
  add_stats(aov_car_aggregate_both) %>%
  add_stats(aov_car_within) %>%
  add_stats(aov_car_no_df_pes) %>%
  add_stats(aov_car_no_df_no_MSE)

# Inspect output
aov_car
aov_car_covariate
aov_car_aggregate
aov_car_aggregate_both
aov_car_within
aov_car_no_df_pes
aov_car_no_df_no_MSE

# aov_4() -----------------------------------------------------------------

# Run analyses
aov_4 <- aov_4(
  value ~ treatment * gender + (phase * hour | id),
  data = obk.long, observed = "gender"
)
aov_4_covariate <- aov_4(
  value ~ treatment * gender + age + (phase * hour | id),
  data = obk.long, observed = c("gender", "age"),
  factorize = FALSE
)
aov_4_aggregate_both <- aov_4(
  value ~ treatment * gender + (1 | id),
  data = obk.long,
  observed = c("gender")
)
aov_4_within <- aov_4(
  value ~ (phase * hour | id),
  data = obk.long
)

# Add stats
statistics <- statistics %>%
  add_stats(aov_4) %>%
  add_stats(aov_4_covariate) %>%
  add_stats(aov_4_aggregate_both) %>%
  add_stats(aov_4_within)

# Inspect output
aov_4
aov_4_covariate
aov_4_aggregate_both
aov_4_within

# mixed() -----------------------------------------------------------------

# Get data
data("Machines", package = "MEMSS")
data(md_15.1)

# Run analyses
m1 <- mixed(score ~ Machine + (Machine | Worker), data = Machines)
m2 <- mixed(
  score ~ Machine + (Machine || Worker),
  data = Machines, expand_re = TRUE
)
t15.4a <- mixed(iq ~ timecat + (1 + time | id), data = md_15.1)
mixed1_orig <- mixed(
  severity ~ sex + (1 | id), md_16.1,
  check_contrasts = FALSE
)

# Add stats


# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/testthat/data/afex.json")

# Cleanup -----------------------------------------------------------------

rm(
  aov_ez, aov_ez_default, aov_ez_aggregate, aov_ez_aggregate_both,
  aov_ez_p, aov_car, aov_car_covariate, aov_ez_covariate, aov_car_aggregate,
  aov_car_aggregate_both, aov_car_within, aov_car_no_df_pes,
  aov_car_no_df_no_MSE, aov_4, aov_4_covariate, aov_4_aggregate_both,
  aov_4_within, df, statistics, md_12.1, obk.long
)

# Issue: #11 --------------------------------------------------------------

library(tidyverse)
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
  contrast("trt.vs.ctrl") %>%
  test(by = NULL)

results <- list()

x <- tidy_stats.summary_emm(emm_results)

results <- results %>%
  add_stats(emm_results)

# Write stats
write_stats(results, "inst/test_data/afex.json")
