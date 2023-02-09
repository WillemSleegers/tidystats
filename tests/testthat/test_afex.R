# Setup -------------------------------------------------------------------

library(afex)

path <- system.file("tests/data/afex.json", package = "tidystats")
expected_statistics <- read_stats(path)

# aov_ez() ----------------------------------------------------------------

test_that("aov_ez works", {
  data(md_12.1)

  model <- aov_ez(
    "id",
    "rt",
    md_12.1,
    within = c("angle", "noise"),
    anova_table = list(correction = "none", es = "none")
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_ez
  )
})

test_that("aov_ez default works", {
  data(md_12.1)

  model <- aov_ez(
    "id",
    "rt",
    md_12.1,
    within = c("angle", "noise")
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_ez_default
  )
})

test_that("aov_ez covariate works", {
  data(obk.long, package = "afex")

  model <- aov_ez(
    "id",
    "value",
    obk.long,
    between = c("treatment", "gender"),
    within = c("phase", "hour"), covariate = "age",
    observed = c("gender", "age"),
    factorize = FALSE
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_ez_covariate
  )
})

test_that("aov_ez aggregate works", {
  data(obk.long, package = "afex")

  model <- aov_ez(
    "id",
    "value",
    obk.long,
    c("treatment", "gender"),
    "hour",
    observed = "gender",
    fun_aggregate = mean
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_ez_aggregate
  )
})

test_that("aov_ez aggregate over both within-subjected factors works", {
  data(obk.long, package = "afex")

  model <- aov_ez(
    "id",
    "value",
    obk.long,
    between = c("treatment", "gender"),
    observed = "gender",
    fun_aggregate = mean
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_ez_aggregate_both
  )
})

test_that("aov_ez p-value adjustment works", {
  data(obk.long, package = "afex")

  model <- aov_ez(
    "id", "value",
    obk.long,
    between = "treatment",
    within = c("phase", "hour"),
    anova_table = list(p_adjust_method = "holm")
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_ez_p
  )
})

test_that("aov_car works", {
  data(obk.long, package = "afex")

  model <- aov_car(
    value ~ treatment * gender + Error(id / (phase * hour)),
    data = obk.long, observed = "gender"
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_car
  )
})

test_that("aov_car covariate works", {
  data(obk.long, package = "afex")

  model <- aov_car(
    value ~ treatment * gender + age + Error(id / (phase * hour)),
    data = obk.long, observed = c("gender", "age"),
    factorize = FALSE
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_car_covariate
  )
})

test_that("aov_car aggregating over one within-subjects works", {
  data(obk.long, package = "afex")

  model <- aov_car(
    value ~ treatment * gender + Error(id / hour),
    data = obk.long, observed = "gender",
    fun_aggregate = mean
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_car_aggregate
  )
})

test_that("aov_car aggregating over both within-subjects works", {
  data(obk.long, package = "afex")

  model <- aov_car(
    value ~ treatment * gender + Error(id),
    data = obk.long, observed = "gender",
    fun_aggregate = mean
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_car_aggregate_both
  )
})

test_that("aov_car only within-subject works", {
  data(obk.long, package = "afex")

  model <- aov_car(
    value ~ Error(id / (phase * hour)),
    data = obk.long
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_car_within
  )
})

test_that("aov_car no df-correctiona and partial eta-squared works", {
  data(obk.long, package = "afex")

  model <- aov_car(
    value ~ treatment * gender + Error(id / (phase * hour)),
    data = obk.long,
    anova_table = list(correction = "none", es = "pes")
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_car_no_df_pes
  )
})

test_that("aov_car no df-correction and no MSE works", {
  data(obk.long, package = "afex")

  model <- aov_car(
    value ~ treatment * gender + Error(id / (phase * hour)),
    data = obk.long, observed = "gender",
    anova_table = list(correction = "none", MSE = FALSE)
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$aov_car_no_df_no_MSE
  )
})

test_that("mixed simple model with random-slopes works", {
  data("Machines", package = "MEMSS")
  
  model <- mixed(
    score ~ Machine + (Machine | Worker), 
    data = Machines)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$mixed
  )
})

test_that("mixed with expanded random effects terms works", {
  data("Machines", package = "MEMSS")
  
  model <- mixed(
    score ~ Machine + (Machine || Worker),
    data = Machines, expand_re = TRUE
  )
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$mixed_expand_RE
  )
})

test_that("mixed with random intercept plus random slope works", {
  data(md_15.1)
  
  model <- mixed(
    iq ~ timecat + (1 + time | id), 
    data = md_15.1)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$mixed_random_interecept
  )
})

test_that("mixed with treatment contrasts checked works", {
  data(md_16.1)
  
  model <- mixed(
    severity ~ sex + (1 | id), 
    data = md_16.1,
    check_contrasts = FALSE
  )
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$mixed_contrast
  )
})


