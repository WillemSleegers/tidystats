# Setup -------------------------------------------------------------------

if (requireNamespace("emmeans", quietly = TRUE)) library(emmeans)

# emmeans() ---------------------------------------------------------------

test_that("emmeans works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  result <- tidy_stats(emmeans(warp_lm, ~ tension | wool))

  expect_equal(result$method, "Estimated marginal means")
  tension_grp <- result$groups[[1]]
  expect_equal(tension_grp$name, "tension")

  # tension L, wool A
  L_A <- tension_grp$groups[[1]]$groups[[1]]$groups[[1]]
  expect_equal(L_A$name, "A")
  expect_equal(L_A$statistics[[1]]$value, 44.55556,  tolerance = 1e-4) # EMM
  expect_equal(L_A$statistics[[2]]$value, 3.646761,  tolerance = 1e-4) # SE

  # tension L, wool B
  L_B <- tension_grp$groups[[1]]$groups[[1]]$groups[[2]]
  expect_equal(L_B$statistics[[1]]$value, 28.22222,  tolerance = 1e-4) # EMM
})

test_that("emmeans poly works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  result <- tidy_stats(emmeans(warp_lm, poly ~ tension | wool, adjust = "sidak"))

  # Has two top-level groups: EMMs and Contrasts
  grp_names <- sapply(result$groups, function(g) g$name)
  expect_true("Estimated marginal means" %in% grp_names)
  expect_true("Contrasts" %in% grp_names)

  # Contrasts group
  contrasts_grp <- result$groups[[which(grp_names == "Contrasts")]]
  contrast_subgrp <- contrasts_grp$groups[[1]] # contrast
  linear_grp <- contrast_subgrp$groups[[1]]    # linear
  wool_grp   <- linear_grp$groups[[1]]         # wool
  A_grp      <- wool_grp$groups[[1]]           # A
  expect_equal(A_grp$statistics[[1]]$value, -20,          tolerance = 1e-6) # estimate
  expect_equal(A_grp$statistics[[3]]$value, -3.877999,    tolerance = 1e-4) # statistic
  expect_equal(A_grp$statistics[[5]]$value, 0.0006397541, tolerance = 1e-4) # p (sidak-adjusted)
})

test_that("emmeans confint works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  result <- tidy_stats(confint(warp_emm, by = "wool", level = .90))

  expect_equal(result$method, "Estimated marginal means")
  tension_grp <- result$groups[[1]]
  L_A <- tension_grp$groups[[1]]$groups[[1]]$groups[[1]]
  expect_equal(L_A$statistics[[1]]$value, 44.55556, tolerance = 1e-4) # EMM
})

# contrast() --------------------------------------------------------------

test_that("emmeans contrast poly works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  result <- tidy_stats(contrast(warp_emm, "poly"))

  expect_equal(result$method, "Contrasts")
  contrast_grp <- result$groups[[1]] # contrast
  linear_grp   <- contrast_grp$groups[[1]] # linear
  wool_grp     <- linear_grp$groups[[1]]   # wool
  A_grp        <- wool_grp$groups[[1]]     # A

  expect_equal(A_grp$statistics[[1]]$value, -20,          tolerance = 1e-6) # estimate
  expect_equal(A_grp$statistics[[3]]$value, -3.877999,    tolerance = 1e-4) # statistic
  expect_equal(A_grp$statistics[[5]]$value, 0.0003199282, tolerance = 1e-4) # p
})

test_that("emmeans contrast pairs works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  result <- tidy_stats(pairs(warp_emm))

  expect_equal(result$method, "Contrasts")
  contrast_grp <- result$groups[[1]] # contrast
  LM_grp       <- contrast_grp$groups[[1]] # L - M
  wool_grp     <- LM_grp$groups[[1]]       # wool
  A_grp        <- wool_grp$groups[[1]]     # A

  expect_equal(A_grp$statistics[[1]]$value, 20.55556,    tolerance = 1e-4) # estimate
  expect_equal(A_grp$statistics[[3]]$value, 3.985721,    tolerance = 1e-4) # statistic
  expect_equal(A_grp$statistics[[5]]$value, 0.0006572745, tolerance = 1e-4) # p
})

test_that("emmeans contrast eff works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  result <- tidy_stats(contrast(
    warp_emm, "eff",
    by = NULL, enhance.levels = c("wool", "tension")
  ))

  expect_equal(result$method, "Contrasts")
  contrast_grp <- result$groups[[1]] # contrast
  expect_equal(length(contrast_grp$groups), 6)
  first_contrast <- contrast_grp$groups[[1]]
  expect_equal(first_contrast$name, "tensionL woolA effect")
  expect_equal(first_contrast$statistics[[1]]$value, 16.40741,    tolerance = 1e-4) # estimate
  expect_equal(first_contrast$statistics[[5]]$value, 6.166056e-05, tolerance = 1e-4) # p
})

test_that("emmeans contrast pairs simple works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  result <- tidy_stats(pairs(warp_emm, simple = "wool"))

  expect_equal(result$method, "Contrasts")
  contrast_grp <- result$groups[[1]]
  AB_grp       <- contrast_grp$groups[[1]] # A - B
  tension_grp  <- AB_grp$groups[[1]]       # tension
  L_grp        <- tension_grp$groups[[1]]  # L

  expect_equal(L_grp$statistics[[1]]$value, 16.33333,   tolerance = 1e-4) # estimate
  expect_equal(L_grp$statistics[[5]]$value, 0.002676803, tolerance = 1e-4) # p
})

test_that("emmeans contrast pairs each works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  result <- tidy_stats(pairs(warp_emm, simple = "each", combine = FALSE))

  grp_names <- sapply(result$groups, function(g) g$name)
  expect_true("simple contrasts for tension" %in% grp_names)
  expect_true("simple contrasts for wool" %in% grp_names)
})

test_that("emmeans contrast pairs each combined works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  result <- tidy_stats(pairs(warp_emm, simple = "each", combine = TRUE))

  expect_equal(result$method, "Contrasts")
  expect_equal(result$groups[[1]]$name, "wool")
})

# test() ------------------------------------------------------------------

test_that("emmeans test works", {
  skip_if_not_installed("emmeans")
  pigs_lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
  pigs_emm <- emmeans(pigs_lm, specs = "percent", type = "response")
  result <- tidy_stats(test(pigs_emm, null = log(35), delta = log(1.10), side = ">"))

  expect_equal(result$method, "Estimated marginal means")
  percent_grp <- result$groups[[1]]
  p9_grp <- percent_grp$groups[[1]]
  expect_equal(p9_grp$statistics[[1]]$value, 31.3529,     tolerance = 1e-3) # estimate
  expect_equal(p9_grp$statistics[[5]]$value, 0.6390358,   tolerance = 1e-4) # p

  p18_grp <- percent_grp$groups[[4]]
  expect_equal(p18_grp$statistics[[5]]$value, 9.051296e-06, tolerance = 1e-4) # p
})

test_that("emmeans testjoint works", {
  skip_if_not_installed("emmeans")
  pigs_lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
  pigs_emm <- emmeans(pigs_lm, specs = "percent", type = "response")
  pigs_con <- contrast(pigs_emm, "consec")
  result <- tidy_stats(test(pigs_con, joint = TRUE))

  expect_equal(result$method, "Estimated marginal means")
  expect_equal(result$statistics[[1]]$value, 7.981,       tolerance = 1e-3) # F
  expect_equal(result$statistics[[2]]$value, 3,           tolerance = 1e-6) # df numerator
  expect_equal(result$statistics[[3]]$value, 23,          tolerance = 1e-6) # df denominator
  expect_equal(result$statistics[[4]]$value, 0.0007997968, tolerance = 1e-4) # p
})

# mvcontrast() ------------------------------------------------------------

test_that("emmeans mvcontrast works", {
  skip_if_not_installed("emmeans")
  moats_lm <- lm(yield ~ Variety + Block, data = MOats)
  moats_emm <- emmeans(moats_lm, ~ Variety | rep.meas)
  model <- mvcontrast(moats_emm, "consec", show.ests = TRUE)

  output <- add_stats(list(), model, class = "emm_list")
  result <- output$model

  grp_names <- sapply(result$groups, function(g) g$name)
  expect_true("estimates" %in% grp_names)
  expect_true("tests" %in% grp_names)

  tests_grp <- result$groups[[which(grp_names == "tests")]]
  contrast_grp <- tests_grp$groups[[1]]
  MG_grp <- contrast_grp$groups[[1]] # Marvellous - Golden Rain
  expect_equal(MG_grp$statistics[[2]]$value, 0.5393277, tolerance = 1e-4) # statistic
  expect_equal(MG_grp$statistics[[5]]$value, 0.9173667, tolerance = 1e-4) # p
})

test_that("emmeans mvcontrast null works", {
  skip_if_not_installed("emmeans")
  moats_lm <- lm(yield ~ Variety + Block, data = MOats)
  moats_emm <- emmeans(moats_lm, ~ Variety | rep.meas)
  result <- tidy_stats(mvcontrast(
    moats_emm,
    "identity",
    name = "Variety",
    null = c(80, 100, 120, 140)
  ))

  expect_equal(result$method, "Estimated marginal means")
  variety_grp <- result$groups[[1]]
  GR_grp <- variety_grp$groups[[1]] # Golden Rain
  expect_equal(GR_grp$name, "Golden Rain")
  expect_equal(GR_grp$statistics[[1]]$value, 10.00055,   tolerance = 1e-3) # estimate
  expect_equal(GR_grp$statistics[[5]]$value, 0.5662859,  tolerance = 1e-4) # p
})

# eff_size() --------------------------------------------------------------

test_that("emmeans eff_size works", {
  skip_if_not_installed("emmeans")
  fiber_lm <- lm(strength ~ diameter + machine, data = fiber)
  fiber_emm <- emmeans(fiber_lm, "machine")
  result <- tidy_stats(eff_size(
    fiber_emm,
    sigma = sigma(fiber_lm), edf = df.residual(fiber_lm)
  ))

  expect_equal(result$method, "Contrasts")
  contrast_grp <- result$groups[[1]]
  AB_grp <- contrast_grp$groups[[1]] # A - B
  expect_equal(AB_grp$statistics[[1]]$value, -0.6500188, tolerance = 1e-4) # estimate
  expect_equal(AB_grp$statistics[[2]]$value, 0.6499828,  tolerance = 1e-4) # SE
})

# emtrends() --------------------------------------------------------------

test_that("emmeans emtrends works", {
  skip_if_not_installed("emmeans")
  fiber_lm <- lm(strength ~ diameter * machine, data = fiber)
  result <- tidy_stats(emtrends(
    fiber_lm, ~ machine | diameter,
    var = "sqrt(diameter)",
    at = list(diameter = c(20, 30))
  ))

  expect_equal(result$method, "Estimated marginal means of linear trends")
  machine_grp <- result$groups[[1]]
  A_grp       <- machine_grp$groups[[1]] # A
  diam_grp    <- A_grp$groups[[1]]       # diameter
  d20_grp     <- diam_grp$groups[[1]]    # 20
  expect_equal(d20_grp$statistics[[1]]$value, 9.879062, tolerance = 1e-4) # estimate
})

test_that("emmeans emtrends poly works", {
  skip_if_not_installed("emmeans")
  mtcars_lm <- lm(
    mpg ~ poly(disp, degree = 2) * (factor(cyl) + factor(am)),
    data = mtcars
  )
  result <- tidy_stats(emtrends(
    mtcars_lm,
    var = "disp",
    cov.reduce = disp ~ factor(cyl)
  ))

  expect_equal(result$method, "Estimated marginal means of linear trends")
  disp_grp <- result$groups[[1]]
  expect_equal(disp_grp$name, "disp")
  expect_equal(length(disp_grp$groups), 3)

  # First disp level (105.1364) -> cyl -> 4 -> am -> 0
  d105_grp <- disp_grp$groups[[1]]
  am0_grp  <- d105_grp$groups[[1]]$groups[[1]]$groups[[1]]$groups[[1]]
  expect_equal(am0_grp$statistics[[1]]$value, -0.09485245, tolerance = 1e-4) # estimate
})

# joint_tests() -----------------------------------------------------------

test_that("emmeans joint_tests works", {
  skip_if_not_installed("emmeans")
  pigs_lm <- lm(log(conc) ~ source * factor(percent), data = pigs)
  result <- tidy_stats(joint_tests(pigs_lm))

  expect_equal(result$method, "Estimated marginal means")
  term_grp <- result$groups[[1]]
  source_grp <- term_grp$groups[[1]]
  expect_equal(source_grp$name, "source")
  expect_equal(source_grp$statistics[[1]]$value, 30.256,      tolerance = 1e-3) # F
  expect_equal(source_grp$statistics[[4]]$value, 2.506905e-06, tolerance = 1e-4) # p
})

test_that("emmeans joint_tests by works", {
  skip_if_not_installed("emmeans")
  pigs_lm <- lm(log(conc) ~ source * factor(percent), data = pigs)
  result <- tidy_stats(joint_tests(pigs_lm, by = "source"))

  expect_equal(result$method, "Estimated marginal means")
  term_grp <- result$groups[[1]]
  percent_grp <- term_grp$groups[[1]]
  expect_equal(percent_grp$name, "percent")

  source_grp <- percent_grp$groups[[1]]
  fish_grp   <- source_grp$groups[[1]]
  expect_equal(fish_grp$name, "fish")
  expect_equal(fish_grp$statistics[[4]]$value, 0.2022617, tolerance = 1e-4) # p
})

# ref_grid() --------------------------------------------------------------

test_that("emmeans ref_grid works", {
  skip_if_not_installed("emmeans")
  fiber_lm <- lm(strength ~ machine * diameter, data = fiber)
  result <- tidy_stats(ref_grid(fiber_lm))

  expect_equal(result$method, "Estimated marginal means")
  machine_grp <- result$groups[[1]]
  A_grp       <- machine_grp$groups[[1]] # A
  expect_equal(A_grp$name, "A")

  diam_grp <- A_grp$groups[[1]]
  d_grp    <- diam_grp$groups[[1]]
  expect_equal(d_grp$statistics[[1]]$value, 40.2221, tolerance = 1e-3) # estimate
})
