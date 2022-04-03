
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)

# Load test data
path <- system.file("tests/testthat/data/htest.json", package = "tidystats")
test_results <- read_stats(path)

# Set options
tolerance <- 0.001

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

