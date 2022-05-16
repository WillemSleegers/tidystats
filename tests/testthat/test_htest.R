
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Load test data
path <- system.file("tests/testthat/data/htest.json", package = "tidystats")
test_results <- read_stats(path)

# Set options
tolerance <- 0.001

# Function to compare models
# model: the model to be passed to tidy_stats
# tidy_model_test: test results to compare to
models_equal = function(model, tidy_model_test) {
  tidy_model <- tidy_stats(model)
  tidy_model$package <- NULL
  tidy_model_test$package <- NULL
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
}

# Test: t.test() ----------------------------------------------------------

test_that("one sample t-test works", {
  model <- t.test(extra ~ 1, data = sleep)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$t_test_one_sample

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("two sample t-test works", {
  model <- t.test(extra ~ group, data = sleep, var.equal = TRUE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$t_test_two_sample

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Welch t-test works", {
  model <- t.test(extra ~ group, data = sleep)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$t_test_welch

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("paired t-test works", {
  model <- t.test(extra ~ group, data = sleep, paired = TRUE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$t_test_paired

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: cor.test() --------------------------------------------------------

x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)

test_that("pearson correlation works", {
  model <- cor.test(x, y, method = "pearson")

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$correlation_pearson

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("spearman correlation works", {
  model <- cor.test(x, y, method = "spearman")

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$correlation_spearman

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("kendall correlation works", {
  model <- cor.test(x, y, method = "kendall")

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$correlation_kendall

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: chisq.test() ------------------------------------------------------

test_that("pearson's chi-squared test works", {
  M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
  dimnames(M) <- list(gender = c("F", "M"), party = c("Democrat","Independent",
  "Republican"))

  model <- chisq.test(M)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$chi_squared

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("pearson's chi-squared test with yates' correction works", {
  x <- matrix(c(12, 5, 7, 7), ncol = 2)

  model <- chisq.test(x)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$chi_squared_yates

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("chi-squared test with for given probabilities works", {
  y <- c(A = 20, B = 15, C = 25)

  model <- chisq.test(y)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$chi_squared_prob

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: prop.test() -------------------------------------------------------

set.seed(1)
heads <- rbinom(1, size = 100, prob = .5)

test_that("1-sample proportion test works", {
  model <- prop.test(heads, 100)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$prop_test

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("1-sample proportion test without continuity correction works", {
  model <- prop.test(heads, 100, correct = FALSE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$prop_test_correct

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("4-sample proportion test works", {
  smokers  <- c(83, 90, 129, 70)
  patients <- c(86, 93, 136, 82)

  model <- prop.test(smokers, patients)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$prop_test_smokers

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: prop.test() -------------------------------------------------------

test_that("Chi-squared Test for Trend in Proportions works",
  {
    models_equal(
      prop.trend.test(smokers, patients),
      test_results$prop_trend_test)
  })

test_that("Chi-squared Test for Trend in Proportions (with alternative scores) works",
  {
    models_equal(
      prop.trend.test(smokers, patients, c(0,0,0,1)),
      test_results$prop_trend_test_scores)
  })


# Test: wilcox.test() -----------------------------------------------------

test_that("wilcoxon signed rank exact test works", {
  x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
  y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

  model <- wilcox.test(x, y, paired = TRUE, alternative = "greater")

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$wilcoxon_signed_rank

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("wilcoxon rank sum tests with continuity correction works", {
  model <- suppressWarnings(
      wilcox.test(Ozone ~ Month, data = airquality, subset = Month %in% c(5, 8))
    )

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$wilcoxon_rank_sum_continuity

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

test_that("wilcoxon rank sum tests works", {
  model <- wilcox.test(x, y, alternative = "greater", exact = FALSE,
    correct = FALSE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$wilcoxon_rank_sum

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("wilcoxon rank sum tests works", {
  model <- wilcox.test(x, y, conf.int = TRUE, conf.level = .9)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$wilcoxon_rank_sum_conf

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: kruskal.test() ----------------------------------------------------

test_that("kruskal-wallis rank sum test works", {
  x <- c(2.9, 3.0, 2.5, 2.6, 3.2)
  y <- c(3.8, 2.7, 4.0, 2.4)
  z <- c(2.8, 3.4, 3.7, 2.2, 2.0)

  model <- kruskal.test(list(x, y, z))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$kruskal

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("kruskal-wallis rank sum test with formula notation works", {
  model <- kruskal.test(Ozone ~ Month, data = airquality)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$kruskal_formula

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: fisher.test() -----------------------------------------------------

test_that("fisher's exact tests works", {
  TeaTasting <- matrix(c(3, 1, 1, 3), nrow = 2)

  model <- fisher.test(TeaTasting, alternative = "greater")

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$fisher_test

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("fisher's exact tests without a confidence interval works", {
  Convictions <- matrix(c(2, 10, 15, 3), nrow = 2)

  model <- fisher.test(Convictions, conf.int = FALSE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$fisher_test_no_CI

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

Job <- matrix(c(1, 2, 1, 0, 3, 3, 6, 1, 10, 10, 14, 9, 6, 7, 12, 11), 4, 4)

test_that("fisher's exact tests on r x c tables works", {
  model <- fisher.test(Job)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$fisher_test_r_by_c

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("fisher's exact tests with simulated p-value works", {
  set.seed(2015)
  model <- fisher.test(Job, simulate.p.value = TRUE, B = 1e5)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$fisher_test_simulated_p

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("fisher's exact tests hybrid works", {
  MP6 <- rbind(
    c(1, 2, 2, 1, 1, 0, 1),
    c(2, 0, 0, 2, 3, 0, 0),
    c(0, 1, 1, 1, 2, 7, 3),
    c(1, 1, 2, 0, 0, 0, 1),
    c(0, 1, 1, 1, 1, 0, 0)
  )

  model <- fisher.test(MP6, hybrid = TRUE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$fisher_test_hybrid

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: ks.test() ---------------------------------------------------------

set.seed(1)
x <- rnorm(50)
y <- runif(30)

test_that("two-sample kolmogorov-smirnov test works", {
  model <- ks.test(x, y)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$ks_test_two

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("one-sample kolmogorov-smirnov test works", {
  model <- ks.test(x + 2, "pgamma", 3, 2)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$ks_test_one

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("inexact kolmogorov-smirnov test works", {
  model <- ks.test(x + 2, "pgamma", 3, 2, exact = FALSE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$ks_test_inexact

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("greater alternative kolmogorov-smirnov test works", {
  model <- ks.test(x + 2, "pgamma", 3, 2, alternative = "greater")

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$ks_test_greater

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: oneway.test() -----------------------------------------------------

test_that("one-way analysis of means (not assuming equal variances) works", {
  model <- oneway.test(extra ~ group, data = sleep)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$oneway_test

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("one-way analysis of means (assuming equal variances) works", {
  model <- oneway.test(extra ~ group, data = sleep, var.equal = TRUE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$oneway_test_equal_var

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: var.test() --------------------------------------------------------

test_that("F test to compare two variances works", {
  set.seed(1)
  x <- rnorm(50, mean = 0, sd = 2)
  y <- rnorm(30, mean = 1, sd = 1)

  model <- var.test(x, y)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$var_test

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

# Test: mauchly.test() --------------------------------------------------------

invisible(capture.output(utils::example(SSD)))

test_that("Mauchly's test of sphericity (traditional) works",
  {
    models_equal(
      mauchly.test(mlmfit, X = ~ 1),
      test_results$mauchly_test)
  })

idata <- data.frame(deg = gl(3, 1, 6, labels = c(0, 4, 8)),
  noise = gl(2, 3, 6, labels = c("A", "P")))

test_that("Mauchly's test of sphericity (inner projection) works",
  {
    models_equal(
      mauchly.test(mlmfit, X = ~ deg + noise, idata = idata),
      test_results$mauchly_test_orthogonal
    )
  })

test_that("Mauchly's test of sphericity (outer projection) works",
  {
    models_equal(
      mauchly.test(
        mlmfit,
        M = ~ deg + noise,
        X = ~ noise,
        idata = idata
      ),
      test_results$mauchly_test_spanned
    )
  })


# Test: mcnemar.test() --------------------------------------------------------

Performance <-
  matrix(
    c(794, 86, 150, 570),
    nrow = 2,
    dimnames = list(
      "1st Survey" = c("Approve", "Disapprove"),
      "2nd Survey" = c("Approve", "Disapprove")
    )
  )

test_that("McNemar's Chi-squared test (with continuity correction) works",
  {
    models_equal(
      mcnemar.test(Performance),
      test_results$mcnemar_test)
  })

test_that("McNemar's Chi-squared test (without continuity correction) works",
  {
    models_equal(
      mcnemar.test(Performance, correct = FALSE),
      test_results$mcnemar_test_nocorrect)
  })


# Test: binom.test() --------------------------------------------------------

test_that("Exact binomial test works",
  {
    models_equal(
      binom.test(c(682, 243)),
      test_results$binom_test)
  })

test_that("Exact binomial test (one-sided) works",
  {
    models_equal(
      binom.test(c(682, 243), p = 3 / 4, alternative = "less"),
      test_results$binom_test_params)
  })


# Test: PP.test() --------------------------------------------------------

set.seed(1)
x <- rnorm(1000)
y <- cumsum(x)

test_that("Phillips-Perron Unit Root Test works",
  {
    models_equal(
      PP.test(x),
      test_results$pp_test)
  })

test_that("Phillips-Perron Unit Root Test (long truncation parameter) works",
  {
    models_equal(
      PP.test(y, lshort = FALSE),
      test_results$pp_test_long)
  })


# Test: Box.test() --------------------------------------------------------

set.seed(1)
x <- rnorm (100)

test_that("Box-Pierce works",
  {
    models_equal(
      Box.test(x, lag = 1),
      test_results$box_test)
  })

test_that("Ljung-Pierce works",
  {
    models_equal(
      Box.test (x, lag = 2, type = "Ljung"),
      test_results$box_test_ljung)
  })


# Test: ansari.test() --------------------------------------------------------

ramsay <- c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99,
  101, 96, 97, 102, 107, 113, 116, 113, 110, 98)
jung.parekh <- c(107, 108, 106, 98, 105, 103, 110, 105, 104,
  100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99)

test_that("Ansari-Bradley Test works",
  {
    models_equal(suppressWarnings(ansari.test(ramsay, jung.parekh)),
     test_results$ansari_test)
  })

test_that("Ansari-Bradley Test (with CI) works",
  {
    set.seed(1)
    models_equal(
      ansari.test(rnorm(100), rnorm(100, 0, 2), conf.int = TRUE),
      test_results$ansari_test_ci)
  })

# Test: mood.test() --------------------------------------------------------

test_that("Mood two-sample test of scale works",
  {
    models_equal(
      mood.test(ramsay, jung.parekh),
      test_results$mood_test)
  })

# Test: quade.test() --------------------------------------------------------

dataFreq <- matrix(c( 5,  4,  7, 10, 12,
     1,  3,  1,  0,  2,
    16, 12, 22, 22, 35,
     5,  4,  3,  5,  4,
    10,  9,  7, 13, 10,
    19, 18, 28, 37, 58,
    10,  7,  6,  8,  7),
  nrow = 7, byrow = TRUE,
  dimnames =
  list(Store = as.character(1:7),
       Brand = LETTERS[1:5]))

test_that("Quade test works",
  {
    models_equal(
      quade.test(dataFreq),
      test_results$quade_test)
  })


# Test: bartlett.test() --------------------------------------------------------

test_that("Bartlett test of homogeneity of variances works",
  {
    models_equal(
      bartlett.test(InsectSprays$count, InsectSprays$spray),
      test_results$bartlett_test)
  })

# Test: fligner.test() --------------------------------------------------------

test_that("Fligner-Killeen test of homogeneity of variances works",
  {
    models_equal(
      fligner.test(InsectSprays$count, InsectSprays$spray),
      test_results$fligner_test)
  })


# Test: poisson.test() --------------------------------------------------------

test_that("Exact Poisson test works",
  {
    models_equal(
      poisson.test(137, 24.19893),
      test_results$poisson_test)
  })

test_that("Comparison of Poisson rates",
  {
    models_equal(
      poisson.test(c(11, 6+8+7), c(800, 1083+1050+878)),
      test_results$poisson_test_comparison)
  })


# Test: shapiro.test() --------------------------------------------------------

set.seed(1)

# Run analysis
test_that("Shapiro-Wilk normality test works",
  {
    models_equal(
      shapiro.test(runif(100, min = 2, max = 4)),
      test_results$shapiro_test)
  })


# Test: friedman.test() --------------------------------------------------------

RoundingTimes <-
  matrix(c(5.40, 5.50, 5.55,
    5.85, 5.70, 5.75,
    5.20, 5.60, 5.50,
    5.55, 5.50, 5.40,
    5.90, 5.85, 5.70,
    5.45, 5.55, 5.60,
    5.40, 5.40, 5.35,
    5.45, 5.50, 5.35,
    5.25, 5.15, 5.00,
    5.85, 5.80, 5.70,
    5.25, 5.20, 5.10,
    5.65, 5.55, 5.45,
    5.60, 5.35, 5.45,
    5.05, 5.00, 4.95,
    5.50, 5.50, 5.40,
    5.45, 5.55, 5.50,
    5.55, 5.55, 5.35,
    5.45, 5.50, 5.55,
    5.50, 5.45, 5.25,
    5.65, 5.60, 5.40,
    5.70, 5.65, 5.55,
    6.30, 6.30, 6.25),
    nrow = 22,
  byrow = TRUE,
  dimnames = list(1:22,
  c("Round Out", "Narrow Angle", "Wide Angle")))

test_that("Friedman rank sum test works",
  {
    models_equal(
      friedman.test(RoundingTimes),
      test_results$friedman_test)
  })


# Test: mantelhaen.test() --------------------------------------------------------

Satisfaction <-
  as.table(array(c(1, 2, 0, 0, 3, 3, 1, 2,
    11, 17, 8, 4, 2, 3, 5, 2,
    1, 0, 0, 0, 1, 3, 0, 1,
    2, 5, 7, 9, 1, 1, 3, 6),
  dim = c(4, 4, 2),
  dimnames =
  list(Income =
  c("<5000", "5000-15000",
  "15000-25000", ">25000"),
  "Job Satisfaction" =
  c("V_D", "L_S", "M_S", "V_S"),
  Gender = c("Female", "Male"))))

Rabbits <-
  array(c(0, 0, 6, 5,
    3, 0, 3, 6,
    6, 2, 0, 4,
    5, 6, 1, 0,
    2, 5, 0, 0),
  dim = c(2, 2, 5),
  dimnames = list(
  Delay = c("None", "1.5h"),
  Response = c("Cured", "Died"),
  Penicillin.Level = c("1/8", "1/4", "1/2", "1", "4")))

test_that("Cochran-Mantel-Haenszel test works",
  {
    models_equal(
      mantelhaen.test(Satisfaction),
      test_results$mantelhaen_test)
  })
test_that("Mantel-Haenszel chi-squared test (with continuity correction) works",
  {
    models_equal(
      mantelhaen.test(Rabbits),
      test_results$mantelhaen_test_2by2)
  })
test_that("Exact conditional test of independence in 2 x 2 x k tables works",
  {
    models_equal(
      mantelhaen.test(Rabbits, exact = TRUE),
      test_results$mantelhaen_test_2by2_exact)
  })
