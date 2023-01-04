
# Setup -------------------------------------------------------------------

# Load test data
path <- system.file("tests/data/htest.json", package = "tidystats")
expected_statistics <- read_stats(path)

# t.test() ----------------------------------------------------------------

test_that("one sample t-test works", {
  model <- t.test(extra ~ 1, data = sleep)

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$t_test_one_sample
  )
})

test_that("two sample t-test works", {
  model <- t.test(extra ~ group, data = sleep, var.equal = TRUE)

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$t_test_two_sample
  )
})

test_that("Welch t-test works", {
  model <- t.test(extra ~ group, data = sleep)

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$t_test_welch
  )
})

test_that("paired t-test works", {
  model <- t.test(extra ~ group, data = sleep, paired = TRUE)

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$t_test_paired
  )
})

# cor.test() --------------------------------------------------------------

test_that("pearson correlation works", {
  x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
  y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)
  
  model <- cor.test(x, y, method = "pearson")

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$correlation_pearson
  )
})

test_that("spearman correlation works", {
  x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
  y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)
  
  model <- cor.test(x, y, method = "spearman")

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$correlation_spearman
  )
})

test_that("kendall correlation works", {
  x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
  y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)
  
  model <- cor.test(x, y, method = "kendall")

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$correlation_kendall
  )
})

# chisq.test() ------------------------------------------------------------

test_that("pearson's chi-squared test works", {
  M <- as.table(
    x = rbind(
      c(762, 327, 468), 
      c(484, 239, 477)
    )
  )
  
  dimnames(M) <- list(
    gender = c("F", "M"), 
    party = c("Democrat","Independent", "Republican")
  )

  model <- chisq.test(M)

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$chi_squared
  )
})

test_that("pearson's chi-squared test with yates' correction works", {
  x <- matrix(c(12, 5, 7, 7), ncol = 2)

  model <- chisq.test(x)

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$chi_squared_yates
  )
})

test_that("chi-squared test with for given probabilities works", {
  y <- c(A = 20, B = 15, C = 25)

  model <- chisq.test(y)

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$chi_squared_prob
  )
})

# prop.test() -------------------------------------------------------------

test_that("1-sample proportion test works", {
  set.seed(1)
  
  heads <- rbinom(1, size = 100, prob = .5)
  
  model <- prop.test(heads, 100)

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$prop_test
  )
})

test_that("1-sample proportion test without continuity correction works", {
  set.seed(1)
  
  heads <- rbinom(1, size = 100, prob = .5)
  
  model <- prop.test(heads, 100, correct = FALSE)

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$prop_test_correct
  )
})

test_that("4-sample proportion test works", {
  smokers  <- c(83, 90, 129, 70)
  patients <- c(86, 93, 136, 82)
  
  model <- prop.test(smokers, patients)

  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$prop_test_smokers
  )
})

# prop.test() -------------------------------------------------------------

test_that("Chi-squared test for trend in proportions works", {
  smokers  <- c(83, 90, 129, 70)
  patients <- c(86, 93, 136, 82)
  
  model <- prop.trend.test(smokers, patients)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$prop_trend_test,
  )
})

test_that(
  paste(
    "Chi-squared test for trend in proportions (with alternative", 
    "scores) works"
  ), {
  smokers  <- c(83, 90, 129, 70)
  patients <- c(86, 93, 136, 82)
  
  model <- prop.trend.test(smokers, patients, c(0, 0, 0, 1))
  
  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$prop_trend_test_scores
  )
})

# wilcox.test() -----------------------------------------------------------

test_that("wilcoxon signed rank exact test works", {
  x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
  y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

  model <- wilcox.test(x, y, paired = TRUE, alternative = "greater")

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$wilcoxon_signed_rank
  )
})

test_that("wilcoxon rank sum tests with continuity correction works", {
  model <- suppressWarnings(
    wilcox.test(Ozone ~ Month, data = airquality, subset = Month %in% c(5, 8))
  )

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$wilcoxon_rank_sum_continuity
  )
})

test_that("wilcoxon rank sum tests works", {
  x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
  y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
  
  model <- wilcox.test(x, y, alternative = "greater", exact = FALSE,
    correct = FALSE)

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$wilcoxon_rank_sum
  )
})

test_that("wilcoxon rank sum tests works", {
  x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
  y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
  
  model <- wilcox.test(x, y, conf.int = TRUE, conf.level = .9)

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$wilcoxon_rank_sum_conf
  )
})

# kruskal.test() ----------------------------------------------------------

test_that("kruskal-wallis rank sum test works", {
  x <- c(2.9, 3.0, 2.5, 2.6, 3.2)
  y <- c(3.8, 2.7, 4.0, 2.4)
  z <- c(2.8, 3.4, 3.7, 2.2, 2.0)

  model <- kruskal.test(list(x, y, z))

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$kruskal
  )
})

test_that("kruskal-wallis rank sum test with formula notation works", {
  model <- kruskal.test(Ozone ~ Month, data = airquality)

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$kruskal_formula
  )
})

# fisher.test() -----------------------------------------------------------

test_that("fisher's exact tests works", {
  TeaTasting <- matrix(c(3, 1, 1, 3), nrow = 2)

  model <- fisher.test(TeaTasting, alternative = "greater")

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$fisher_test
  )
})

test_that("fisher's exact tests without a confidence interval works", {
  Convictions <- matrix(c(2, 10, 15, 3), nrow = 2)

  model <- fisher.test(Convictions, conf.int = FALSE)

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$fisher_test_no_CI
  )
})

test_that("fisher's exact tests on r x c tables works", {
  Job <- matrix(c(1, 2, 1, 0, 3, 3, 6, 1, 10, 10, 14, 9, 6, 7, 12, 11), 4, 4)
  
  model <- fisher.test(Job)

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$fisher_test_r_by_c
  )
})

test_that("fisher's exact tests with simulated p-value works", {
  set.seed(2015)
  
  Job <- matrix(c(1, 2, 1, 0, 3, 3, 6, 1, 10, 10, 14, 9, 6, 7, 12, 11), 4, 4)
  
  model <- fisher.test(Job, simulate.p.value = TRUE, B = 1e5)

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$fisher_test_simulated_p
  )
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

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$fisher_test_hybrid
  )
})

# ks.test() ---------------------------------------------------------------

test_that("two-sample kolmogorov-smirnov test works", {
  set.seed(1)
  
  x <- rnorm(50)
  y <- runif(30)
  
  model <- ks.test(x, y)
  
  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$ks_test_two
  )
})

test_that("one-sample kolmogorov-smirnov test works", {
  set.seed(1)
  
  x <- rnorm(50)
  y <- runif(30)
  
  model <- ks.test(x + 2, "pgamma", 3, 2)

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$ks_test_one
  )
})

test_that("inexact kolmogorov-smirnov test works", {
  set.seed(1)
  
  x <- rnorm(50)
  y <- runif(30)
  
  model <- ks.test(x + 2, "pgamma", 3, 2, exact = FALSE)

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$ks_test_inexact
  )
})

test_that("greater alternative kolmogorov-smirnov test works", {
  set.seed(1)
  
  x <- rnorm(50)
  y <- runif(30)
  
  model <- ks.test(x + 2, "pgamma", 3, 2, alternative = "greater")

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$ks_test_greater
  )
})

# oneway.test() -----------------------------------------------------------

test_that("one-way analysis of means (not assuming equal variances) works", {
  model <- oneway.test(extra ~ group, data = sleep)

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$oneway_test
  )
})

test_that("one-way analysis of means (assuming equal variances) works", {
  model <- oneway.test(extra ~ group, data = sleep, var.equal = TRUE)

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$oneway_test_equal_var
  )
})

# var.test() --------------------------------------------------------------

test_that("F test to compare two variances works", {
  set.seed(1)

  x <- rnorm(50, mean = 0, sd = 2)
  y <- rnorm(30, mean = 1, sd = 1)

  model <- var.test(x, y)

  expect_equal_models(
    model = model,  
    expected_tidy_model = expected_statistics$var_test
  )
})

# mauchly.test() ----------------------------------------------------------

test_that("Mauchly's test of sphericity (traditional) works", {
  invisible(capture.output(utils::example(SSD)))
  
  model <- mauchly.test(mlmfit, X = ~ 1)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$mauchly_test
  )
})

test_that("Mauchly's test of sphericity (inner projection) works", {
  invisible(capture.output(utils::example(SSD)))
  
  idata <- data.frame(
    deg = gl(3, 1, 6, labels = c(0, 4, 8)),
    noise = gl(2, 3, 6, labels = c("A", "P"))
  )
  
  model <- mauchly.test(mlmfit, X = ~ deg + noise, idata = idata)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$mauchly_test_orthogonal
  )
})

test_that("Mauchly's test of sphericity (outer projection) works", {
  invisible(capture.output(utils::example(SSD)))
  
  idata <- data.frame(
    deg = gl(3, 1, 6, labels = c(0, 4, 8)),
    noise = gl(2, 3, 6, labels = c("A", "P"))
  )
  
  model <- mauchly.test(mlmfit, M = ~ deg + noise, X = ~ noise, idata = idata)
    
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$mauchly_test_spanned
  )
})

# mcnemar.test() ----------------------------------------------------------

test_that("McNemar's Chi-squared test (with continuity correction) works", {
  Performance <- matrix(
    data = c(794, 86, 150, 570),
    nrow = 2,
    dimnames = list(
      "1st Survey" = c("Approve", "Disapprove"),
      "2nd Survey" = c("Approve", "Disapprove")
    )
  )
  
  model <- mcnemar.test(Performance)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$mcnemar_test
  )
})

test_that("McNemar's Chi-squared test (without continuity correction) works", {
  Performance <- matrix(
    data = c(794, 86, 150, 570),
    nrow = 2,
    dimnames = list(
      "1st Survey" = c("Approve", "Disapprove"),
      "2nd Survey" = c("Approve", "Disapprove")
    )
  )
  
  model <- mcnemar.test(Performance, correct = FALSE)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$mcnemar_test_nocorrect
  )
})

# binom.test() ------------------------------------------------------------

test_that("Exact binomial test works", {
  model <- binom.test(c(682, 243))
    
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$binom_test
  )
})

test_that("Exact binomial test (one-sided) works", {
  model <- binom.test(c(682, 243), p = 3 / 4, alternative = "less")
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$binom_test_params
  )
})

# PP.test() ---------------------------------------------------------------

test_that("Phillips-Perron unit root test works", {
  set.seed(1)
  
  x <- rnorm(1000)
  y <- cumsum(x)
  
  model <- PP.test(x)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$pp_test
  )
})

test_that("Phillips-Perron unit root test (long truncation parameter) works", {
  set.seed(1)
  
  x <- rnorm(1000)
  y <- cumsum(x)
  
  model <- PP.test(y, lshort = FALSE)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$pp_test_long
  )
})

# Box.test() --------------------------------------------------------------

test_that("Box-Pierce works", {
  set.seed(1)

  x <- rnorm (100)
  
  model <- Box.test(x, lag = 1)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$box_test
  )
})

test_that("Ljung-Pierce works", {
  set.seed(1)

  x <- rnorm (100)
  
  model <- Box.test (x, lag = 2, type = "Ljung")
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$box_test_ljung
  )
})

# ansari.test() -----------------------------------------------------------

test_that("Ansari-Bradley test works", {
  ramsay <- c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99, 101, 96, 97, 102, 
  107, 113, 116, 113, 110, 98)
  jung_parekh <- c(107, 108, 106, 98, 105, 103, 110, 105, 104, 100, 96, 108, 
    103, 104, 114, 114, 113, 108, 106, 99)
  
  model <- suppressWarnings(ansari.test(ramsay, jung_parekh))
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$ansari_test
  )
})

test_that("Ansari-Bradley test (with CI) works", {
  set.seed(1)
  
  model <- ansari.test(rnorm(100), rnorm(100, 0, 2), conf.int = TRUE)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$ansari_test_ci
  )
})

# mood.test() -------------------------------------------------------------

test_that("Mood two-sample test of scale works", {
  ramsay <- c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99, 101, 96, 97, 102, 
    107, 113, 116, 113, 110, 98)
  jung_parekh <- c(107, 108, 106, 98, 105, 103, 110, 105, 104, 100, 96, 108, 
    103, 104, 114, 114, 113, 108, 106, 99)
  
  model <- mood.test(ramsay, jung_parekh)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$mood_test
  )
})

# quade.test() ------------------------------------------------------------

test_that("Quade test works", {
  dataFreq <- matrix(
    nrow = 7, 
    byrow = TRUE,
    data = c(5, 4, 7, 10, 12,
      1,  3,  1,  0,  2,
      16, 12, 22, 22, 35,
      5, 4, 3, 5, 4,
      10, 9, 7, 13, 10,
      19, 18, 28, 37, 58,
      10, 7, 6, 8, 7
    ),
    dimnames = list(Store = as.character(1:7), Brand = LETTERS[1:5])
  )
  
  model <- quade.test(dataFreq)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$quade_test
  )
})

# bartlett.test() ---------------------------------------------------------

test_that("Bartlett test of homogeneity of variances works", {
  model <- bartlett.test(InsectSprays$count, InsectSprays$spray)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$bartlett_test
  )
})

# fligner.test() ----------------------------------------------------------

test_that("Fligner-Killeen test of homogeneity of variances works", {
  model <- fligner.test(InsectSprays$count, InsectSprays$spray)
      
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$fligner_test
  )
})

# poisson.test() ----------------------------------------------------------

test_that("Exact Poisson test works", {
  model <- poisson.test(137, 24.19893)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$poisson_test
  )
})

test_that("Comparison of Poisson rates", {
  model <- poisson.test(c(11, 6 + 8 + 7), c(800, 1083 + 1050 + 878))
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$poisson_test_comparison
  )
})

# shapiro.test() ----------------------------------------------------------

test_that("Shapiro-Wilk normality test works", {
  set.seed(1)
  
  model <- shapiro.test(runif(100, min = 2, max = 4))
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$shapiro_test
  )
})

# friedman.test() ---------------------------------------------------------

test_that("Friedman rank sum test works", {
  rounding_times <- matrix(
    nrow = 22,
    byrow = TRUE,
    data = c(
      5.40, 5.50, 5.55,
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
      6.30, 6.30, 6.25
    ),
    dimnames = list(1:22, c("Round Out", "Narrow Angle", "Wide Angle"))
  )
  
  model <- friedman.test(rounding_times)
  
  expect_equal_models(
    model = model,
    expected_tidy_model = expected_statistics$friedman_test
  )
})

# mantelhaen.test() -------------------------------------------------------

test_that("Cochran-Mantel-Haenszel test works", {
  Satisfaction <- as.table(
    array(
      dim = c(4, 4, 2),
      data = c(
        1, 2, 0, 0, 3, 3, 1, 2,
        11, 17, 8, 4, 2, 3, 5, 2,
        1, 0, 0, 0, 1, 3, 0, 1,
        2, 5, 7, 9, 1, 1, 3, 6
      ),
      dimnames = list(
        Income = c("<5000", "5000-15000", "15000-25000", ">25000"), 
        `Job Satisfaction` = c("V_D", "L_S", "M_S", "V_S"),
        Gender = c("Female", "Male")
      )
    )
  )
  
  model <- mantelhaen.test(Satisfaction)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$mantelhaen_test
  )
})

test_that("Mantel-Haenszel chi-squared test (with continuity correction) works", 
  {
    Rabbits <- array(
      c(
        0, 0, 6, 5,
        3, 0, 3, 6,
        6, 2, 0, 4,
        5, 6, 1, 0,
        2, 5, 0, 0
      ),
      dim = c(2, 2, 5),
      dimnames = list(
      Delay = c("None", "1.5h"),
      Response = c("Cured", "Died"),
      Penicillin.Level = c("1/8", "1/4", "1/2", "1", "4"))
    )
    
    model <- mantelhaen.test(Rabbits)
    
    expect_equal_models(
      model = model, 
      expected_tidy_model = expected_statistics$mantelhaen_test_2by2
    )
  }
)

test_that("Exact conditional test of independence in 2 x 2 x k tables works", {
  Rabbits <- array(
    c(
      0, 0, 6, 5,
      3, 0, 3, 6,
      6, 2, 0, 4,
      5, 6, 1, 0,
      2, 5, 0, 0
    ),
    dim = c(2, 2, 5),
    dimnames = list(
    Delay = c("None", "1.5h"),
    Response = c("Cured", "Died"),
    Penicillin.Level = c("1/8", "1/4", "1/2", "1", "4"))
  )
  
  model <- mantelhaen.test(Rabbits, exact = TRUE)
  
  expect_equal_models(
    model = model, 
    expected_tidy_model = expected_statistics$mantelhaen_test_2by2_exact
  )
})
