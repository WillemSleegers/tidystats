# Notes -------------------------------------------------------------------

# Could store more statistics by running `summary()` on the output of these
# functions.

# Setup -------------------------------------------------------------------

library(afex)

statistics <- list()

# aov_ez() ----------------------------------------------------------------

data(md_12.1)
data(obk.long, package = "afex")

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
  observed = "gender",
  fun_aggregate = mean
)
aov_ez_aggregate_both <- aov_ez(
  "id",
  "value",
  obk.long,
  between = c("treatment", "gender"),
  observed = "gender",
  fun_aggregate = mean
)
aov_ez_p <- aov_ez(
  "id", "value",
  obk.long,
  between = "treatment",
  within = c("phase", "hour"),
  anova_table = list(p_adjust_method = "holm")
)

statistics <- statistics |>
  add_stats(aov_ez) |>
  add_stats(aov_ez_default) |>
  add_stats(aov_ez_covariate) |>
  add_stats(aov_ez_aggregate) |>
  add_stats(aov_ez_aggregate_both) |>
  add_stats(aov_ez_p)

aov_ez
aov_ez_default
aov_ez_covariate
aov_ez_aggregate
aov_ez_aggregate_both
aov_ez_p

# aov_car() ---------------------------------------------------------------

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
  data = obk.long, observed = "gender",
  fun_aggregate = mean
)
aov_car_aggregate_both <- aov_car(
  value ~ treatment * gender + Error(id),
  data = obk.long, observed = "gender",
  fun_aggregate = mean
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

statistics <- statistics |>
  add_stats(aov_car) |>
  add_stats(aov_car_covariate) |>
  add_stats(aov_car_aggregate) |>
  add_stats(aov_car_aggregate_both) |>
  add_stats(aov_car_within) |>
  add_stats(aov_car_no_df_pes) |>
  add_stats(aov_car_no_df_no_MSE)

aov_car
aov_car_covariate
aov_car_aggregate
aov_car_aggregate_both
aov_car_within
aov_car_no_df_pes
aov_car_no_df_no_MSE

# aov_4() -----------------------------------------------------------------

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
  observed = c("gender"),
  fun_aggregate = mean
)
aov_4_within <- aov_4(
  value ~ (phase * hour | id),
  data = obk.long
)

statistics <- statistics |>
  add_stats(aov_4) |>
  add_stats(aov_4_covariate) |>
  add_stats(aov_4_aggregate_both) |>
  add_stats(aov_4_within)

aov_4
aov_4_covariate
aov_4_aggregate_both
aov_4_within

# mixed() -----------------------------------------------------------------

data("Machines", package = "MEMSS")
data(md_15.1)
data(md_16.1)

mixed <- mixed(
  score ~ Machine + (Machine | Worker),
  data = Machines
)

mixed_expand_RE <- mixed(
  score ~ Machine + (Machine || Worker),
  data = Machines, expand_re = TRUE
)

mixed_random_interecept <- mixed(
  iq ~ timecat + (1 + time | id),
  data = md_15.1
)

mixed_contrast <- mixed(
  severity ~ sex + (1 | id),
  data = md_16.1,
  check_contrasts = FALSE
)

statistics <- statistics |>
  add_stats(mixed) |>
  add_stats(mixed_expand_RE) |>
  add_stats(mixed_random_interecept) |>
  add_stats(mixed_contrast)

mixed
mixed_expand_RE
mixed_random_interecept
mixed_contrast

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/afex.json")

# Cleanup -----------------------------------------------------------------

rm(
  aov_ez, aov_ez_default, aov_ez_aggregate, aov_ez_aggregate_both,
  aov_ez_p, aov_car, aov_car_covariate, aov_ez_covariate, aov_car_aggregate,
  aov_car_aggregate_both, aov_car_within, aov_car_no_df_pes,
  aov_car_no_df_no_MSE, aov_4, aov_4_covariate, aov_4_aggregate_both,
  aov_4_within, mixed, mixed_expand_RE, mixed_random_interecept,
  mixed_contrast, df, statistics, md_12.1, obk.long, md_15.1, md_16.1,
  Machines
)
