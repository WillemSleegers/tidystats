# Direct tests ------------------------------------------------------------

get_pair_p <- function(result, i) {
  result$groups[[1]]$groups[[i]]$statistics[[1]]$value
}

test_that("pairwise.t.test extracts correct p-values", {
  Month <- factor(airquality$Month, labels = month.abb[5:9])
  result <- tidy_stats(pairwise.t.test(airquality$Ozone, Month))

  expect_equal(result$method, "Pairwise t tests with pooled SD")
  expect_equal(result$p_adjust_method, "holm")
  expect_equal(length(result$groups[[1]]$groups), 10) # 5 choose 2
  expect_equal(get_pair_p(result, 1), 1,            tolerance = 1e-6) # Jun vs May
  expect_equal(get_pair_p(result, 2), 0.0002638036, tolerance = 1e-6) # Jul vs May
  expect_equal(get_pair_p(result, 3), 0.0001949061, tolerance = 1e-6) # Aug vs May
  expect_equal(get_pair_p(result, 4), 1,            tolerance = 1e-6) # Sep vs May
  expect_equal(get_pair_p(result, 5), 0.05112741,   tolerance = 1e-6) # Jul vs Jun
  expect_equal(get_pair_p(result, 6), 0.04987333,   tolerance = 1e-6) # Aug vs Jun
  expect_equal(get_pair_p(result, 7), 1,            tolerance = 1e-6) # Sep vs Jun
  expect_equal(get_pair_p(result, 8), 1,            tolerance = 1e-6) # Aug vs Jul
  expect_equal(get_pair_p(result, 9), 0.004878798,  tolerance = 1e-6) # Sep vs Jul
  expect_equal(get_pair_p(result, 10), 0.003878108, tolerance = 1e-6) # Sep vs Aug
})

test_that("pairwise.t.test with non-pooled SD extracts correct p-values", {
  Month <- factor(airquality$Month, labels = month.abb[5:9])
  result <- tidy_stats(pairwise.t.test(
    airquality$Ozone,
    Month,
    p.adjust.method = "bonf",
    pool.sd = FALSE
  ))

  expect_equal(result$method, "Pairwise t tests with non-pooled SD")
  expect_equal(result$p_adjust_method, "bonferroni")
  expect_equal(get_pair_p(result, 1), 1,            tolerance = 1e-6) # Jun vs May
  expect_equal(get_pair_p(result, 2), 0.0002646679, tolerance = 1e-6) # Jul vs May
  expect_equal(get_pair_p(result, 3), 0.002168566,  tolerance = 1e-6) # Aug vs May
  expect_equal(get_pair_p(result, 6), 0.04269318,   tolerance = 1e-6) # Aug vs Jun
  expect_equal(get_pair_p(result, 9), 0.007361032,  tolerance = 1e-6) # Sep vs Jul
})

test_that("pairwise.t.test paired extracts correct p-values", {
  result <- tidy_stats(pairwise.t.test(
    c(1, 2, 3, 1, 2, 4),
    c(1, 1, 2, 2, 3, 3),
    paired = TRUE
  ))

  expect_equal(result$method, "Pairwise paired t tests")
  expect_equal(length(result$groups[[1]]$groups), 3) # 3 choose 2
  expect_equal(get_pair_p(result, 1), 1,          tolerance = 1e-6) # 2 vs 1
  expect_equal(get_pair_p(result, 2), 0.6144983,  tolerance = 1e-6) # 3 vs 1
  expect_equal(get_pair_p(result, 3), 1,          tolerance = 1e-6) # 3 vs 2
})

# pairwise.prop.test() ----------------------------------------------------

test_that("pairwise.prop.test extracts correct p-values", {
  smokers <- c(83, 90, 129, 70)
  patients <- c(86, 93, 136, 82)
  result <- tidy_stats(suppressWarnings(pairwise.prop.test(smokers, patients)))

  expect_equal(result$method, "Pairwise comparison of proportions")
  expect_equal(length(result$groups[[1]]$groups), 6) # 4 choose 2
  expect_equal(get_pair_p(result, 1), 1,           tolerance = 1e-6) # 2 vs 1
  expect_equal(get_pair_p(result, 2), 1,           tolerance = 1e-6) # 3 vs 1
  expect_equal(get_pair_p(result, 3), 0.1185648,   tolerance = 1e-6) # 4 vs 1
  expect_equal(get_pair_p(result, 4), 1,           tolerance = 1e-6) # 3 vs 2
  expect_equal(get_pair_p(result, 5), 0.09321728,  tolerance = 1e-6) # 4 vs 2
  expect_equal(get_pair_p(result, 6), 0.1237680,   tolerance = 1e-6) # 4 vs 3
})

# pairwise.wilcox.test() --------------------------------------------------

test_that("pairwise.wilcox.test extracts correct p-value", {
  result <- tidy_stats(suppressWarnings(pairwise.wilcox.test(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11),
    c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
  )))

  expect_equal(result$method, "Pairwise Wilcoxon rank sum exact test")
  # 2 groups = 1 pair, stored directly in statistics (no groups nesting)
  expect_equal(result$statistics[[1]]$value, 0.007936508, tolerance = 1e-6)
})

test_that("pairwise.wilcox.test paired extracts correct p-values", {
  result <- tidy_stats(pairwise.wilcox.test(
    PlantGrowth$weight,
    PlantGrowth$group,
    p.adjust.method = "BH",
    paired = TRUE
  ))

  expect_equal(result$method, "Pairwise Wilcoxon signed rank exact test")
  expect_equal(length(result$groups[[1]]$groups), 3) # 3 choose 2
  expect_equal(get_pair_p(result, 1), 0.375,       tolerance = 1e-6) # trt1 vs ctrl
  expect_equal(get_pair_p(result, 2), 0.1962891,   tolerance = 1e-6) # trt2 vs ctrl
  expect_equal(get_pair_p(result, 3), 0.05859375,  tolerance = 1e-6) # trt2 vs trt1
})
