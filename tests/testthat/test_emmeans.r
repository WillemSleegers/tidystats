# Setup -------------------------------------------------------------------

expected_statistics <- read_stats("../data/emmeans.json")

library(emmeans)

# emmeans() ---------------------------------------------------------------

test_that("emmeans works", {
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  model <- emmeans(warp_lm, ~ tension | wool)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$warp_emm
  )
})

test_that("emmeans poly works", {
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  model <- emmeans(warp_lm, poly ~ tension | wool, adjust = "sidak")

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$warp_emm_poly
  )
})

test_that("emmeans confint works", {
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  model <- confint(warp_emm, by = "wool", level = .90)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$warp_emm_confint
  )
})

# contrast() --------------------------------------------------------------

test_that("emmeans contrast poly works", {
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  model <- contrast(warp_emm, "poly")

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$warp_contrast_poly
  )
})

test_that("emmeans contrast pairs works", {
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  model <- pairs(warp_emm)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$warp_pairs
  )
})

test_that("emmeans contrast eff works", {
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  model <- contrast(
    warp_emm, "eff",
    by = NULL, enhance.levels = c("wool", "tension")
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$warp_contrast_eff
  )
})

test_that("emmeans contrast pairs simple works", {
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  model <- pairs(warp_emm, simple = "wool")

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$warp_pairs_simple
  )
})

test_that("emmeans contrast pairs each works", {
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  model <- pairs(warp_emm, simple = "each", combine = FALSE)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$warp_pairs_each
  )
})

test_that("emmeans contrast pairs each combined works", {
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  model <- pairs(warp_emm, simple = "each", combine = TRUE)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$warp_pairs_each_combined
  )
})

# test() ------------------------------------------------------------------

test_that("emmeans test works", {
  pigs_lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
  pigs_emm <- emmeans(pigs_lm, specs = "percent", type = "response")
  model <- test(pigs_emm, null = log(35), delta = log(1.10), side = ">")

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$pigs_test
  )
})

test_that("emmeans testjoint works", {
  pigs_lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
  pigs_emm <- emmeans(pigs_lm, specs = "percent", type = "response")
  pigs_con <- contrast(pigs_emm, "consec")
  model <- test(pigs_con, joint = TRUE)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$pigs_test_joint
  )
})

# mvcontrast() ------------------------------------------------------------

test_that("emmeans mvcontrast works", {
  moats_lm <- lm(yield ~ Variety + Block, data = MOats)
  moats_emm <- emmeans(moats_lm, ~ Variety | rep.meas)
  model <- mvcontrast(moats_emm, "consec", show.ests = TRUE)

  output <- add_stats(list(), model, class = "emm_list")
  object <- output$model
  object$package <- NULL
  expected <- expected_statistics$moats_mvcontrast
  expected$package <- NULL

  expect_equal(
    object = object,
    expected = expected, tolerance = 0.0001
  )
})

test_that("emmeans mvcontrast null works", {
  moats_lm <- lm(yield ~ Variety + Block, data = MOats)
  moats_emm <- emmeans(moats_lm, ~ Variety | rep.meas)
  model <- mvcontrast(
    moats_emm,
    "identity",
    name = "Variety",
    null = c(80, 100, 120, 140)
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$moats_mvcontrast_null
  )
})

# eff_size() --------------------------------------------------------------

test_that("emmeans eff_size works", {
  fiber_lm <- lm(strength ~ diameter + machine, data = fiber)
  fiber_emm <- emmeans(fiber_lm, "machine")
  model <- eff_size(
    fiber_emm,
    sigma = sigma(fiber_lm), edf = df.residual(fiber_lm)
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$fiber_eff_size
  )
})

# emtrends() --------------------------------------------------------------

test_that("emmeans emtrends works", {
  fiber_lm <- lm(strength ~ diameter * machine, data = fiber)
  model <- emtrends(
    fiber_lm, ~ machine | diameter,
    var = "sqrt(diameter)",
    at = list(diameter = c(20, 30))
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$fiber_trends
  )
})

test_that("emmeans emtrends poly works", {
  mtcars_lm <- lm(
    mpg ~ poly(disp, degree = 2) * (factor(cyl) + factor(am)),
    data = mtcars
  )
  model <- emtrends(
    mtcars_lm,
    var = "disp",
    cov.reduce = disp ~ factor(cyl)
  )

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$mtcars_trends
  )
})

# joint_tests() -----------------------------------------------------------

test_that("emmeans emtrends works", {
  pigs_lm <- lm(log(conc) ~ source * factor(percent), data = pigs)
  model <- joint_tests(pigs_lm)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$pigs_joint_tests
  )
})

test_that("emmeans emtrends by works", {
  pigs_lm <- lm(log(conc) ~ source * factor(percent), data = pigs)
  model <- joint_tests(pigs_lm, by = "source")

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$pigs_joint_tests_by
  )
})

# ref_grid() --------------------------------------------------------------

test_that("emmeans ref_grid works", {
  fiber_lm <- lm(strength ~ machine * diameter, data = fiber)
  model <- ref_grid(fiber_lm)

  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$fiber_ref_grid
  )
})
