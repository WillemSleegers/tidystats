if (requireNamespace("afex", quietly = TRUE)) library(afex)

# aov_ez() ----------------------------------------------------------------

test_that("aov_ez works", {
  skip_if_not_installed("afex")
  data(md_12.1)

  result <- tidy_stats(suppressMessages(aov_ez(
    "id",
    "rt",
    md_12.1,
    within = c("angle", "noise"),
    anova_table = list(correction = "none", es = "none")
  )))

  terms <- result$groups[[1]]$groups
  expect_equal(terms[[1]]$name, "angle")
  expect_equal(terms[[1]]$statistics[[4]]$value, 40.7191,      tolerance = 1e-3) # F
  expect_equal(terms[[1]]$statistics[[5]]$value, 2.086763e-07, tolerance = 1e-4) # p
  expect_equal(terms[[2]]$name, "noise")
  expect_equal(terms[[2]]$statistics[[4]]$value, 33.76596,     tolerance = 1e-3) # F
  expect_equal(terms[[3]]$name, "angle:noise")
  expect_equal(terms[[3]]$statistics[[4]]$value, 45.31034,     tolerance = 1e-3) # F
})

test_that("aov_ez default works", {
  skip_if_not_installed("afex")
  data(md_12.1)

  result <- tidy_stats(suppressMessages(aov_ez(
    "id",
    "rt",
    md_12.1,
    within = c("angle", "noise")
  )))

  terms <- result$groups[[1]]$groups
  expect_equal(terms[[1]]$name, "angle")
  expect_equal(terms[[1]]$statistics[[1]]$value, 1.923273, tolerance = 1e-4) # GG df num
  expect_equal(terms[[1]]$statistics[[4]]$value, 40.7191,  tolerance = 1e-3) # F
  expect_equal(terms[[1]]$statistics[[5]]$value, 0.3901179, tolerance = 1e-4) # ges
})

test_that("aov_ez covariate works", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")

  result <- tidy_stats(suppressMessages(aov_ez(
    "id",
    "value",
    obk.long,
    between = c("treatment", "gender"),
    within = c("phase", "hour"), covariate = "age",
    observed = c("gender", "age"),
    factorize = FALSE
  )))

  expect_equal(result$method, "ANOVA")
  expect_true(length(result$groups[[1]]$groups) > 0)
})

test_that("aov_ez aggregate works", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")

  result <- tidy_stats(suppressMessages(aov_ez(
    "id",
    "value",
    obk.long,
    c("treatment", "gender"),
    "hour",
    observed = "gender",
    fun_aggregate = mean
  )))

  expect_equal(result$method, "ANOVA")
  expect_true(length(result$groups[[1]]$groups) > 0)
})

test_that("aov_ez aggregate over both within-subjected factors works", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")

  result <- tidy_stats(suppressMessages(aov_ez(
    "id",
    "value",
    obk.long,
    between = c("treatment", "gender"),
    observed = "gender",
    fun_aggregate = mean
  )))

  expect_equal(result$method, "ANOVA")
})

test_that("aov_ez p-value adjustment works", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")

  result <- tidy_stats(suppressMessages(aov_ez(
    "id", "value",
    obk.long,
    between = "treatment",
    within = c("phase", "hour"),
    anova_table = list(p_adjust_method = "holm")
  )))

  expect_equal(result$method, "ANOVA")
})

test_that("aov_car works", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")

  result <- tidy_stats(suppressMessages(aov_car(
    value ~ treatment * gender + Error(id / (phase * hour)),
    data = obk.long, observed = "gender"
  )))

  expect_equal(result$method, "ANOVA")
  expect_true(length(result$groups[[1]]$groups) > 0)
})

test_that("aov_car covariate works", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")

  result <- tidy_stats(suppressMessages(aov_car(
    value ~ treatment * gender + age + Error(id / (phase * hour)),
    data = obk.long, observed = c("gender", "age"),
    factorize = FALSE
  )))

  expect_equal(result$method, "ANOVA")
})

test_that("aov_car aggregating over one within-subjects works", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")

  result <- tidy_stats(suppressMessages(aov_car(
    value ~ treatment * gender + Error(id / hour),
    data = obk.long, observed = "gender",
    fun_aggregate = mean
  )))

  expect_equal(result$method, "ANOVA")
})

test_that("aov_car aggregating over both within-subjects works", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")

  result <- tidy_stats(suppressMessages(aov_car(
    value ~ treatment * gender + Error(id),
    data = obk.long, observed = "gender",
    fun_aggregate = mean
  )))

  expect_equal(result$method, "ANOVA")
})

test_that("aov_car only within-subject works", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")

  result <- tidy_stats(suppressMessages(aov_car(
    value ~ Error(id / (phase * hour)),
    data = obk.long
  )))

  expect_equal(result$method, "ANOVA")
})

test_that("aov_car no df-correction and partial eta-squared works", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")

  result <- tidy_stats(suppressMessages(aov_car(
    value ~ treatment * gender + Error(id / (phase * hour)),
    data = obk.long,
    anova_table = list(correction = "none", es = "pes")
  )))

  expect_equal(result$method, "ANOVA")
})

test_that("aov_car no df-correction and no MSE works", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")

  result <- tidy_stats(suppressMessages(aov_car(
    value ~ treatment * gender + Error(id / (phase * hour)),
    data = obk.long, observed = "gender",
    anova_table = list(correction = "none", MSE = FALSE)
  )))

  expect_equal(result$method, "ANOVA")
})

test_that("mixed simple model with random-slopes works", {
  skip_if_not_installed("afex")
  data("Machines", package = "nlme")

  result <- tidy_stats(suppressMessages(mixed(
    score ~ Machine + (Machine | Worker),
    data = Machines, progress = FALSE
  )))

  expect_equal(result$method, "Mixed Model ANOVA")
  terms <- result$groups[[1]]$groups
  expect_equal(terms[[1]]$name, "Machine")
  expect_equal(terms[[1]]$statistics[[3]]$value, 40.98922,     tolerance = 1e-3) # F
  expect_equal(terms[[1]]$statistics[[4]]$value, 0.0007933269, tolerance = 1e-4) # p
})

test_that("mixed with expanded random effects terms works", {
  skip_if_not_installed("afex")
  data("Machines", package = "nlme")

  result <- tidy_stats(suppressMessages(mixed(
    score ~ Machine + (Machine || Worker),
    data = Machines, expand_re = TRUE, progress = FALSE
  )))

  expect_equal(result$method, "Mixed Model ANOVA")
  expect_true(length(result$groups[[1]]$groups) > 0)
})

test_that("mixed with random intercept plus random slope works", {
  skip_if_not_installed("afex")
  data(md_15.1)

  result <- tidy_stats(suppressMessages(mixed(
    iq ~ timecat + (1 + time | id),
    data = md_15.1, progress = FALSE
  )))

  expect_equal(result$method, "Mixed Model ANOVA")
})

test_that("mixed with treatment contrasts checked works", {
  skip_if_not_installed("afex")
  data(md_16.1)

  result <- tidy_stats(suppressMessages(mixed(
    severity ~ sex + (1 | id),
    data = md_16.1, check_contrasts = FALSE, progress = FALSE
  )))

  expect_equal(result$method, "Mixed Model ANOVA")
})
