# Setup -------------------------------------------------------------------

if (requireNamespace("emmeans", quietly = TRUE)) library(emmeans)

# These tests compare against the emmeans object's own summary data frame
# rather than hard-coded constants, so they verify tidy_stats's extraction
# independent of the emmeans version. collect_values() walks the tidied tree
# and gathers every leaf value with a given statistic name; comparing that
# multiset against the matching summary column checks that tidy_stats surfaced
# exactly the values emmeans reports, regardless of the nesting structure.

collect_values <- function(node, stat_name) {
  vals <- numeric(0)
  if (!is.null(node$statistics)) {
    for (s in node$statistics) {
      if (!is.null(s$name) && s$name == stat_name && !is.null(s$value)) {
        vals <- c(vals, s$value)
      }
    }
  }
  if (!is.null(node$groups)) {
    for (g in node$groups) {
      vals <- c(vals, collect_values(g, stat_name))
    }
  }
  vals
}

# emmeans() ---------------------------------------------------------------

test_that("emmeans works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  emm <- emmeans(warp_lm, ~ tension | wool)
  result <- tidy_stats(emm)
  s <- as.data.frame(summary(emm))

  expect_equal(result$method, "Estimated marginal means")
  tension_grp <- result$groups[[1]]
  expect_equal(tension_grp$name, "tension")
  expect_equal(tension_grp$groups[[1]]$groups[[1]]$groups[[1]]$name, "A")

  expect_equal(sort(collect_values(result, "EMM")), sort(s$emmean))
  expect_equal(sort(collect_values(result, "SE")), sort(s$SE))
})

test_that("emmeans poly works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  emm <- emmeans(warp_lm, poly ~ tension | wool, adjust = "sidak")
  result <- tidy_stats(emm)

  # Has two top-level groups: EMMs and Contrasts
  grp_names <- sapply(result$groups, function(g) g$name)
  expect_true("Estimated marginal means" %in% grp_names)
  expect_true("Contrasts" %in% grp_names)

  contrasts_grp <- result$groups[[which(grp_names == "Contrasts")]]
  s <- as.data.frame(summary(emm[["contrasts"]]))
  expect_equal(sort(collect_values(contrasts_grp, "estimate")), sort(s$estimate))
  expect_equal(sort(collect_values(contrasts_grp, "statistic")), sort(s$t.ratio))
  expect_equal(sort(collect_values(contrasts_grp, "p")), sort(s$p.value))
})

test_that("emmeans confint works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  ci <- confint(warp_emm, by = "wool", level = .90)
  result <- tidy_stats(ci)
  s <- as.data.frame(ci)

  expect_equal(result$method, "Estimated marginal means")
  expect_equal(sort(collect_values(result, "EMM")), sort(s$emmean))
})

# contrast() --------------------------------------------------------------

test_that("emmeans contrast poly works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  con <- contrast(warp_emm, "poly")
  result <- tidy_stats(con)
  s <- as.data.frame(summary(con))

  expect_equal(result$method, "Contrasts")
  expect_equal(sort(collect_values(result, "estimate")), sort(s$estimate))
  expect_equal(sort(collect_values(result, "statistic")), sort(s$t.ratio))
  expect_equal(sort(collect_values(result, "p")), sort(s$p.value))
})

test_that("emmeans contrast pairs works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  pr <- pairs(warp_emm)
  result <- tidy_stats(pr)
  s <- as.data.frame(summary(pr))

  expect_equal(result$method, "Contrasts")
  expect_equal(sort(collect_values(result, "estimate")), sort(s$estimate))
  expect_equal(sort(collect_values(result, "statistic")), sort(s$t.ratio))
  expect_equal(sort(collect_values(result, "p")), sort(s$p.value))
})

test_that("emmeans contrast eff works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  con <- contrast(
    warp_emm, "eff",
    by = NULL, enhance.levels = c("wool", "tension")
  )
  result <- tidy_stats(con)
  s <- as.data.frame(summary(con))

  expect_equal(result$method, "Contrasts")
  contrast_grp <- result$groups[[1]] # contrast
  expect_equal(length(contrast_grp$groups), 6)
  expect_equal(contrast_grp$groups[[1]]$name, "tensionL woolA effect")

  expect_equal(sort(collect_values(result, "estimate")), sort(s$estimate))
  expect_equal(sort(collect_values(result, "p")), sort(s$p.value))
})

test_that("emmeans contrast pairs simple works", {
  skip_if_not_installed("emmeans")
  warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
  warp_emm <- emmeans(warp_lm, ~ tension | wool)
  pr <- pairs(warp_emm, simple = "wool")
  result <- tidy_stats(pr)
  s <- as.data.frame(summary(pr))

  expect_equal(result$method, "Contrasts")
  expect_equal(sort(collect_values(result, "estimate")), sort(s$estimate))
  expect_equal(sort(collect_values(result, "p")), sort(s$p.value))
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
  tst <- test(pigs_emm, null = log(35), delta = log(1.10), side = ">")
  result <- tidy_stats(tst)
  s <- as.data.frame(tst)

  expect_equal(result$method, "Estimated marginal means")
  expect_equal(sort(collect_values(result, "estimate")), sort(s$response))
  expect_equal(sort(collect_values(result, "p")), sort(s$p.value))
})

test_that("emmeans testjoint works", {
  skip_if_not_installed("emmeans")
  pigs_lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
  pigs_emm <- emmeans(pigs_lm, specs = "percent", type = "response")
  pigs_con <- contrast(pigs_emm, "consec")
  jt <- test(pigs_con, joint = TRUE)
  result <- tidy_stats(jt)
  s <- as.data.frame(jt)

  expect_equal(result$method, "Estimated marginal means")
  expect_equal(result$statistics[[1]]$value, s$F.ratio)  # F
  expect_equal(result$statistics[[2]]$value, s$df1)      # df numerator
  expect_equal(result$statistics[[3]]$value, s$df2)      # df denominator
  expect_equal(result$statistics[[4]]$value, s$p.value)  # p
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
  s <- as.data.frame(summary(model[["tests"]]))
  expect_equal(sort(collect_values(tests_grp, "statistic")), sort(s$F.ratio))
  expect_equal(sort(collect_values(tests_grp, "p")), sort(s$p.value))
})

test_that("emmeans mvcontrast null works", {
  skip_if_not_installed("emmeans")
  moats_lm <- lm(yield ~ Variety + Block, data = MOats)
  moats_emm <- emmeans(moats_lm, ~ Variety | rep.meas)
  mvc <- mvcontrast(
    moats_emm,
    "identity",
    name = "Variety",
    null = c(80, 100, 120, 140)
  )
  result <- tidy_stats(mvc)
  s <- as.data.frame(summary(mvc))

  expect_equal(result$method, "Estimated marginal means")
  expect_equal(result$groups[[1]]$groups[[1]]$name, "Golden Rain")
  expect_equal(sort(collect_values(result, "estimate")), sort(s$T.square))
  expect_equal(sort(collect_values(result, "p")), sort(s$p.value))
})

# eff_size() --------------------------------------------------------------

test_that("emmeans eff_size works", {
  skip_if_not_installed("emmeans")
  fiber_lm <- lm(strength ~ diameter + machine, data = fiber)
  fiber_emm <- emmeans(fiber_lm, "machine")
  es <- eff_size(
    fiber_emm,
    sigma = sigma(fiber_lm), edf = df.residual(fiber_lm)
  )
  result <- tidy_stats(es)
  s <- as.data.frame(summary(es))

  expect_equal(result$method, "Contrasts")
  expect_equal(sort(collect_values(result, "estimate")), sort(s$effect.size))
  expect_equal(sort(collect_values(result, "SE")), sort(s$SE))
})

# emtrends() --------------------------------------------------------------

test_that("emmeans emtrends works", {
  skip_if_not_installed("emmeans")
  fiber_lm <- lm(strength ~ diameter * machine, data = fiber)
  et <- emtrends(
    fiber_lm, ~ machine | diameter,
    var = "sqrt(diameter)",
    at = list(diameter = c(20, 30))
  )
  result <- tidy_stats(et)
  s <- as.data.frame(summary(et))

  expect_equal(result$method, "Estimated marginal means of linear trends")
  expect_equal(
    sort(collect_values(result, "estimate")),
    sort(s[["sqrt(diameter).trend"]])
  )
})

test_that("emmeans emtrends poly works", {
  skip_if_not_installed("emmeans")
  mtcars_lm <- lm(
    mpg ~ poly(disp, degree = 2) * (factor(cyl) + factor(am)),
    data = mtcars
  )
  et <- emtrends(
    mtcars_lm,
    var = "disp",
    cov.reduce = disp ~ factor(cyl)
  )
  result <- tidy_stats(et)
  s <- as.data.frame(summary(et))

  expect_equal(result$method, "Estimated marginal means of linear trends")
  disp_grp <- result$groups[[1]]
  expect_equal(disp_grp$name, "disp")
  expect_equal(length(disp_grp$groups), 3)

  expect_equal(sort(collect_values(result, "estimate")), sort(s$disp.trend))
})

# joint_tests() -----------------------------------------------------------

test_that("emmeans joint_tests works", {
  skip_if_not_installed("emmeans")
  pigs_lm <- lm(log(conc) ~ source * factor(percent), data = pigs)
  jt <- joint_tests(pigs_lm)
  result <- tidy_stats(jt)
  s <- as.data.frame(jt)

  expect_equal(result$method, "Estimated marginal means")
  expect_equal(result$groups[[1]]$groups[[1]]$name, "source")
  expect_equal(sort(collect_values(result, "statistic")), sort(s$F.ratio))
  expect_equal(sort(collect_values(result, "p")), sort(s$p.value))
})

test_that("emmeans joint_tests by works", {
  skip_if_not_installed("emmeans")
  pigs_lm <- lm(log(conc) ~ source * factor(percent), data = pigs)
  jt <- joint_tests(pigs_lm, by = "source")
  result <- tidy_stats(jt)
  s <- as.data.frame(jt)

  expect_equal(result$method, "Estimated marginal means")
  expect_equal(result$groups[[1]]$groups[[1]]$name, "percent")
  expect_equal(sort(collect_values(result, "p")), sort(s$p.value))
})

# ref_grid() --------------------------------------------------------------

test_that("emmeans ref_grid works", {
  skip_if_not_installed("emmeans")
  fiber_lm <- lm(strength ~ machine * diameter, data = fiber)
  rg <- ref_grid(fiber_lm)
  result <- tidy_stats(rg)
  s <- as.data.frame(summary(rg))

  expect_equal(result$method, "Estimated marginal means")
  expect_equal(result$groups[[1]]$groups[[1]]$name, "A")
  expect_equal(sort(collect_values(result, "estimate")), sort(s$prediction))
})
