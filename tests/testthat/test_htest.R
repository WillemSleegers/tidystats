# t.test() ----------------------------------------------------------------

test_that("one sample t-test works", {
  result <- tidy_stats(t.test(extra ~ 1, data = sleep))

  expect_equal(result$method, "One Sample t-test")
  expect_equal(result$statistics[[3]]$value, 3.412965,   tolerance = 1e-4) # t
  expect_equal(result$statistics[[4]]$value, 19,          tolerance = 1e-6) # df
  expect_equal(result$statistics[[5]]$value, 0.00291762, tolerance = 1e-4) # p
})

test_that("two sample t-test works", {
  result <- tidy_stats(t.test(extra ~ group, data = sleep, var.equal = TRUE))

  expect_equal(result$method, "Two Sample t-test")
  expect_equal(result$statistics[[3]]$value, -1.860813,  tolerance = 1e-4) # t
  expect_equal(result$statistics[[4]]$value, 18,          tolerance = 1e-6) # df
  expect_equal(result$statistics[[5]]$value, 0.07918671, tolerance = 1e-4) # p
})

test_that("Welch t-test works", {
  result <- tidy_stats(t.test(extra ~ group, data = sleep))

  expect_equal(result$method, "Welch Two Sample t-test")
  expect_equal(result$statistics[[3]]$value, -1.860813,  tolerance = 1e-4) # t
  expect_equal(result$statistics[[4]]$value, 17.77647,   tolerance = 1e-4) # df
  expect_equal(result$statistics[[5]]$value, 0.07939414, tolerance = 1e-4) # p
})

test_that("paired t-test works", {
  result <- tidy_stats(t.test(
    sleep$extra[sleep$group == 1],
    sleep$extra[sleep$group == 2],
    paired = TRUE
  ))

  expect_equal(result$method, "Paired t-test")
  expect_equal(result$statistics[[3]]$value, -4.062128,  tolerance = 1e-4) # t
  expect_equal(result$statistics[[4]]$value, 9,           tolerance = 1e-6) # df
  expect_equal(result$statistics[[5]]$value, 0.00283289, tolerance = 1e-4) # p
})

# cor.test() --------------------------------------------------------------

test_that("pearson correlation works", {
  x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
  y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)

  result <- tidy_stats(cor.test(x, y, method = "pearson"))

  expect_equal(result$statistics[[1]]$value, 0.5711816,  tolerance = 1e-4) # r
  expect_equal(result$statistics[[3]]$value, 7,           tolerance = 1e-6) # df
  expect_equal(result$statistics[[4]]$value, 0.1081731,  tolerance = 1e-4) # p
})

test_that("spearman correlation works", {
  x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
  y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)

  result <- tidy_stats(cor.test(x, y, method = "spearman"))

  expect_equal(result$statistics[[1]]$value, 0.6,         tolerance = 1e-4) # rho
  expect_equal(result$statistics[[2]]$value, 48,          tolerance = 1e-6) # S
  expect_equal(result$statistics[[3]]$value, 0.09679784, tolerance = 1e-4) # p
})

test_that("kendall correlation works", {
  x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
  y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)

  result <- tidy_stats(cor.test(x, y, method = "kendall"))

  expect_equal(result$statistics[[1]]$value, 0.4444444, tolerance = 1e-4) # tau
  expect_equal(result$statistics[[2]]$value, 26,         tolerance = 1e-6) # T
  expect_equal(result$statistics[[3]]$value, 0.1194389, tolerance = 1e-4) # p
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
    party = c("Democrat", "Independent", "Republican")
  )

  result <- tidy_stats(chisq.test(M))

  expect_equal(result$method, "Pearson's Chi-squared test")
  expect_equal(result$statistics[[1]]$value, 30.07015,      tolerance = 1e-4) # X2
  expect_equal(result$statistics[[2]]$value, 2,              tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 2.953589e-07,  tolerance = 1e-4) # p
})

test_that("pearson's chi-squared test with yates' correction works", {
  x <- matrix(c(12, 5, 7, 7), ncol = 2)

  result <- tidy_stats(chisq.test(x))

  expect_equal(result$statistics[[1]]$value, 0.6411203, tolerance = 1e-4) # X2
  expect_equal(result$statistics[[2]]$value, 1,          tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 0.4233054, tolerance = 1e-4) # p
})

test_that("chi-squared test with for given probabilities works", {
  y <- c(A = 20, B = 15, C = 25)

  result <- tidy_stats(chisq.test(y))

  expect_equal(result$statistics[[1]]$value, 2.5,       tolerance = 1e-6) # X2
  expect_equal(result$statistics[[2]]$value, 2,          tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 0.2865048, tolerance = 1e-4) # p
})

# prop.test() -------------------------------------------------------------

test_that("1-sample proportion test works", {
  set.seed(1)

  heads <- rbinom(1, size = 100, prob = .5)

  result <- tidy_stats(prop.test(heads, 100))

  expect_equal(result$statistics[[2]]$value, 0.09,      tolerance = 1e-6) # X2
  expect_equal(result$statistics[[3]]$value, 1,          tolerance = 1e-6) # df
  expect_equal(result$statistics[[4]]$value, 0.7641772, tolerance = 1e-4) # p
})

test_that("1-sample proportion test without continuity correction works", {
  set.seed(1)

  heads <- rbinom(1, size = 100, prob = .5)

  result <- tidy_stats(prop.test(heads, 100, correct = FALSE))

  expect_equal(result$statistics[[2]]$value, 0.16,      tolerance = 1e-6) # X2
  expect_equal(result$statistics[[4]]$value, 0.6891565, tolerance = 1e-4) # p
})

test_that("4-sample proportion test works", {
  smokers <- c(83, 90, 129, 70)
  patients <- c(86, 93, 136, 82)

  result <- tidy_stats(prop.test(smokers, patients))

  expect_equal(result$statistics[[1]]$value, 12.60041,    tolerance = 1e-4) # X2
  expect_equal(result$statistics[[2]]$value, 3,            tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 0.005585477, tolerance = 1e-4) # p
})

# prop.trend.test() -------------------------------------------------------

test_that("Chi-squared test for trend in proportions works", {
  smokers <- c(83, 90, 129, 70)
  patients <- c(86, 93, 136, 82)

  result <- tidy_stats(prop.trend.test(smokers, patients))

  expect_equal(result$statistics[[1]]$value, 8.224922,   tolerance = 1e-4) # X2
  expect_equal(result$statistics[[2]]$value, 1,           tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 0.004131897, tolerance = 1e-4) # p
})

test_that(
  paste(
    "Chi-squared test for trend in proportions (with alternative",
    "scores) works"
  ),
  {
    smokers <- c(83, 90, 129, 70)
    patients <- c(86, 93, 136, 82)

    result <- tidy_stats(prop.trend.test(smokers, patients, c(0, 0, 0, 1)))

    expect_equal(result$statistics[[1]]$value, 12.17315,     tolerance = 1e-4) # X2
    expect_equal(result$statistics[[3]]$value, 0.0004848246, tolerance = 1e-4) # p
  }
)

# wilcox.test() -----------------------------------------------------------

test_that("wilcoxon signed rank exact test works", {
  x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
  y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

  result <- tidy_stats(wilcox.test(x, y, paired = TRUE, alternative = "greater"))

  expect_equal(result$method, "Wilcoxon signed rank exact test")
  expect_equal(result$statistics[[1]]$value, 40,          tolerance = 1e-6) # W
  expect_equal(result$statistics[[2]]$value, 0.01953125, tolerance = 1e-4) # p
})

test_that("wilcoxon rank sum tests with continuity correction works", {
  result <- tidy_stats(suppressWarnings(
    wilcox.test(Ozone ~ Month, data = airquality, subset = Month %in% c(5, 8))
  ))

  expect_equal(result$statistics[[1]]$value, 127.5,        tolerance = 1e-6) # W
  expect_equal(result$statistics[[2]]$value, 0.0001208078, tolerance = 1e-4) # p
})

test_that("wilcoxon rank sum tests works", {
  x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
  y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

  result <- tidy_stats(wilcox.test(x, y,
    alternative = "greater", exact = FALSE,
    correct = FALSE
  ))

  expect_equal(result$statistics[[1]]$value, 35,       tolerance = 1e-6) # W
  expect_equal(result$statistics[[2]]$value, 0.1103357, tolerance = 1e-4) # p
})

test_that("wilcoxon rank sum tests with confidence interval works", {
  x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
  y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

  result <- tidy_stats(wilcox.test(x, y, conf.int = TRUE, conf.level = .9))

  expect_equal(result$statistics[[1]]$value, 0.305,     tolerance = 1e-4) # estimate
  expect_equal(result$statistics[[2]]$value, 35,         tolerance = 1e-6) # W
  expect_equal(result$statistics[[3]]$value, 0.2544123, tolerance = 1e-4) # p
})

# kruskal.test() ----------------------------------------------------------

test_that("kruskal-wallis rank sum test works", {
  x <- c(2.9, 3.0, 2.5, 2.6, 3.2)
  y <- c(3.8, 2.7, 4.0, 2.4)
  z <- c(2.8, 3.4, 3.7, 2.2, 2.0)

  result <- tidy_stats(kruskal.test(list(x, y, z)))

  expect_equal(result$method, "Kruskal-Wallis rank sum test")
  expect_equal(result$statistics[[1]]$value, 0.7714286, tolerance = 1e-4) # H
  expect_equal(result$statistics[[2]]$value, 2,          tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 0.6799648, tolerance = 1e-4) # p
})

test_that("kruskal-wallis rank sum test with formula notation works", {
  result <- tidy_stats(kruskal.test(Ozone ~ Month, data = airquality))

  expect_equal(result$statistics[[1]]$value, 29.26658,     tolerance = 1e-4) # H
  expect_equal(result$statistics[[2]]$value, 4,             tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 6.900714e-06, tolerance = 1e-4) # p
})

# fisher.test() -----------------------------------------------------------

test_that("fisher's exact tests works", {
  TeaTasting <- matrix(c(3, 1, 1, 3), nrow = 2)

  result <- tidy_stats(fisher.test(TeaTasting, alternative = "greater"))

  expect_equal(result$statistics[[1]]$value, 6.408309,  tolerance = 1e-4) # OR
  expect_equal(result$statistics[[2]]$value, 0.2428571, tolerance = 1e-4) # p
})

test_that("fisher's exact tests without a confidence interval works", {
  Convictions <- matrix(c(2, 10, 15, 3), nrow = 2)

  result <- tidy_stats(fisher.test(Convictions, conf.int = FALSE))

  expect_equal(result$statistics[[1]]$value, 0.04693661,   tolerance = 1e-4) # OR
  expect_equal(result$statistics[[2]]$value, 0.0005367241, tolerance = 1e-4) # p
})

test_that("fisher's exact tests on r x c tables works", {
  Job <- matrix(c(1, 2, 1, 0, 3, 3, 6, 1, 10, 10, 14, 9, 6, 7, 12, 11), 4, 4)

  result <- tidy_stats(fisher.test(Job))

  expect_equal(result$statistics[[1]]$value, 0.7826849, tolerance = 1e-4) # p
})

test_that("fisher's exact tests with simulated p-value works", {
  set.seed(2015)

  Job <- matrix(c(1, 2, 1, 0, 3, 3, 6, 1, 10, 10, 14, 9, 6, 7, 12, 11), 4, 4)

  result <- tidy_stats(fisher.test(Job, simulate.p.value = TRUE, B = 1e5))

  expect_equal(result$statistics[[1]]$value, 0.7833222, tolerance = 1e-4) # p
})

test_that("fisher's exact tests hybrid works", {
  MP6 <- rbind(
    c(1, 2, 2, 1, 1, 0, 1),
    c(2, 0, 0, 2, 3, 0, 0),
    c(0, 1, 1, 1, 2, 7, 3),
    c(1, 1, 2, 0, 0, 0, 1),
    c(0, 1, 1, 1, 1, 0, 0)
  )

  result <- tidy_stats(fisher.test(MP6, hybrid = TRUE))

  expect_equal(result$statistics[[1]]$value, 0.03928964, tolerance = 1e-4) # p
})

# ks.test() ---------------------------------------------------------------

test_that("two-sample kolmogorov-smirnov test works", {
  set.seed(1)

  x <- rnorm(50)
  y <- runif(30)

  result <- tidy_stats(ks.test(x, y))

  expect_equal(result$statistics[[1]]$value, 0.48,       tolerance = 1e-6) # D
  expect_equal(result$statistics[[2]]$value, 0.000203307, tolerance = 1e-4) # p
})

test_that("one-sample kolmogorov-smirnov test works", {
  set.seed(1)

  x <- rnorm(50)

  result <- tidy_stats(ks.test(x + 2, "pgamma", 3, 2))

  expect_equal(result$statistics[[1]]$value, 0.4096183,   tolerance = 1e-4) # D
  expect_equal(result$statistics[[2]]$value, 4.226692e-08, tolerance = 1e-4) # p
})

test_that("inexact kolmogorov-smirnov test works", {
  set.seed(1)

  x <- rnorm(50)

  result <- tidy_stats(ks.test(x + 2, "pgamma", 3, 2, exact = FALSE))

  expect_equal(result$statistics[[1]]$value, 0.4096183,   tolerance = 1e-4) # D
  expect_equal(result$statistics[[2]]$value, 1.033062e-07, tolerance = 1e-4) # p
})

test_that("greater alternative kolmogorov-smirnov test works", {
  set.seed(1)

  x <- rnorm(50)

  result <- tidy_stats(ks.test(x + 2, "pgamma", 3, 2, alternative = "greater"))

  expect_equal(result$statistics[[1]]$value, 0.03999842, tolerance = 1e-4) # D
  expect_equal(result$statistics[[2]]$value, 0.8301601,  tolerance = 1e-4) # p
})

# oneway.test() -----------------------------------------------------------

test_that("one-way analysis of means (not assuming equal variances) works", {
  result <- tidy_stats(oneway.test(extra ~ group, data = sleep))

  expect_equal(result$statistics[[1]]$value, 3.462627,  tolerance = 1e-4) # F
  expect_equal(result$statistics[[3]]$value, 17.77647,  tolerance = 1e-4) # df denominator
  expect_equal(result$statistics[[4]]$value, 0.07939414, tolerance = 1e-4) # p
})

test_that("one-way analysis of means (assuming equal variances) works", {
  result <- tidy_stats(oneway.test(extra ~ group, data = sleep, var.equal = TRUE))

  expect_equal(result$statistics[[1]]$value, 3.462627,  tolerance = 1e-4) # F
  expect_equal(result$statistics[[3]]$value, 18,         tolerance = 1e-6) # df denominator
  expect_equal(result$statistics[[4]]$value, 0.07918671, tolerance = 1e-4) # p
})

# var.test() --------------------------------------------------------------

test_that("F test to compare two variances works", {
  set.seed(1)

  x <- rnorm(50, mean = 0, sd = 2)
  y <- rnorm(30, mean = 1, sd = 1)

  result <- tidy_stats(var.test(x, y))

  expect_equal(result$statistics[[2]]$value, 2.652168,  tolerance = 1e-4) # F
  expect_equal(result$statistics[[3]]$value, 49,         tolerance = 1e-6) # df numerator
  expect_equal(result$statistics[[4]]$value, 29,         tolerance = 1e-6) # df denominator
  expect_equal(result$statistics[[5]]$value, 0.00623206, tolerance = 1e-4) # p
})

# mauchly.test() ----------------------------------------------------------

test_that("Mauchly's test of sphericity (traditional) works", {
  invisible(capture.output(utils::example(SSD)))

  result <- tidy_stats(mauchly.test(mlmfit, X = ~1))

  expect_equal(result$statistics[[1]]$value, 0.03108446, tolerance = 1e-4) # W
  expect_equal(result$statistics[[2]]$value, 0.04765187, tolerance = 1e-4) # p
})

test_that("Mauchly's test of sphericity (inner projection) works", {
  invisible(capture.output(utils::example(SSD)))

  idata <- data.frame(
    deg = gl(3, 1, 6, labels = c(0, 4, 8)),
    noise = gl(2, 3, 6, labels = c("A", "P"))
  )

  result <- tidy_stats(mauchly.test(mlmfit, X = ~ deg + noise, idata = idata))

  expect_equal(result$statistics[[1]]$value, 0.8937772, tolerance = 1e-4) # W
  expect_equal(result$statistics[[2]]$value, 0.6381418, tolerance = 1e-4) # p
})

test_that("Mauchly's test of sphericity (outer projection) works", {
  invisible(capture.output(utils::example(SSD)))

  idata <- data.frame(
    deg = gl(3, 1, 6, labels = c(0, 4, 8)),
    noise = gl(2, 3, 6, labels = c("A", "P"))
  )

  result <- tidy_stats(mauchly.test(mlmfit, M = ~ deg + noise, X = ~noise, idata = idata))

  expect_equal(result$statistics[[1]]$value, 0.960106,  tolerance = 1e-4) # W
  expect_equal(result$statistics[[2]]$value, 0.8497219, tolerance = 1e-4) # p
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

  result <- tidy_stats(mcnemar.test(Performance))

  expect_equal(result$method, "McNemar's Chi-squared test")
  expect_equal(result$statistics[[1]]$value, 16.8178,      tolerance = 1e-4) # X2
  expect_equal(result$statistics[[2]]$value, 1,             tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 4.114562e-05, tolerance = 1e-4) # p
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

  result <- tidy_stats(mcnemar.test(Performance, correct = FALSE))

  expect_equal(result$statistics[[1]]$value, 17.35593,     tolerance = 1e-4) # X2
  expect_equal(result$statistics[[2]]$value, 1,             tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 3.099293e-05, tolerance = 1e-4) # p
})

# binom.test() ------------------------------------------------------------

test_that("Exact binomial test works", {
  result <- tidy_stats(binom.test(c(682, 243)))

  expect_equal(result$statistics[[1]]$value, 0.7372973,   tolerance = 1e-4) # estimate
  expect_equal(result$statistics[[2]]$value, 682,          tolerance = 1e-6) # x
  expect_equal(result$statistics[[3]]$value, 925,          tolerance = 1e-6) # n
  expect_equal(result$statistics[[4]]$value, 7.087291e-49, tolerance = 1e-4) # p
})

test_that("Exact binomial test (one-sided) works", {
  result <- tidy_stats(binom.test(c(682, 243), p = 3 / 4, alternative = "less"))

  expect_equal(result$statistics[[4]]$value, 0.1960093, tolerance = 1e-4) # p
})

# PP.test() ---------------------------------------------------------------

test_that("Phillips-Perron unit root test works", {
  set.seed(1)

  x <- rnorm(1000)

  result <- tidy_stats(PP.test(x))

  expect_equal(result$statistics[[1]]$value, -33.05668, tolerance = 1e-4) # statistic
  expect_equal(result$statistics[[2]]$value, 7,          tolerance = 1e-6) # truncation lag
  expect_equal(result$statistics[[3]]$value, 0.01,       tolerance = 1e-6) # p
})

test_that("Phillips-Perron unit root test (long truncation parameter) works", {
  set.seed(1)

  x <- rnorm(1000)
  y <- cumsum(x)

  result <- tidy_stats(PP.test(y, lshort = FALSE))

  expect_equal(result$statistics[[1]]$value, -2.724437, tolerance = 1e-4) # statistic
  expect_equal(result$statistics[[2]]$value, 21,         tolerance = 1e-6) # truncation lag
  expect_equal(result$statistics[[3]]$value, 0.2716547, tolerance = 1e-4) # p
})

# Box.test() --------------------------------------------------------------

test_that("Box-Pierce works", {
  set.seed(1)

  x <- rnorm(100)

  result <- tidy_stats(Box.test(x, lag = 1))

  expect_equal(result$statistics[[1]]$value, 0.001333163, tolerance = 1e-4) # X2
  expect_equal(result$statistics[[2]]$value, 1,            tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 0.9708737,   tolerance = 1e-4) # p
})

test_that("Ljung-Pierce works", {
  set.seed(1)

  x <- rnorm(100)

  result <- tidy_stats(Box.test(x, lag = 2, type = "Ljung"))

  expect_equal(result$statistics[[1]]$value, 0.07736707, tolerance = 1e-4) # X2
  expect_equal(result$statistics[[2]]$value, 2,           tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 0.9620551,  tolerance = 1e-4) # p
})

# ansari.test() -----------------------------------------------------------

test_that("Ansari-Bradley test works", {
  ramsay <- c(
    111, 107, 100, 99, 102, 106, 109, 108, 104, 99, 101, 96, 97, 102,
    107, 113, 116, 113, 110, 98
  )
  jung_parekh <- c(
    107, 108, 106, 98, 105, 103, 110, 105, 104, 100, 96, 108,
    103, 104, 114, 114, 113, 108, 106, 99
  )

  result <- tidy_stats(suppressWarnings(ansari.test(ramsay, jung_parekh)))

  expect_equal(result$statistics[[1]]$value, 185.5,     tolerance = 1e-4) # C
  expect_equal(result$statistics[[2]]$value, 0.1814582, tolerance = 1e-4) # p
})

test_that("Ansari-Bradley test (with CI) works", {
  set.seed(1)

  result <- tidy_stats(ansari.test(rnorm(100), rnorm(100, 0, 2), conf.int = TRUE))

  expect_equal(result$statistics[[1]]$value, 0.4615525,  tolerance = 1e-4) # estimate
  expect_equal(result$statistics[[2]]$value, 6100,        tolerance = 1e-6) # AB
  expect_equal(result$statistics[[3]]$value, 2.87734e-07, tolerance = 1e-4) # p
})

# mood.test() -------------------------------------------------------------

test_that("Mood two-sample test of scale works", {
  ramsay <- c(
    111, 107, 100, 99, 102, 106, 109, 108, 104, 99, 101, 96, 97, 102,
    107, 113, 116, 113, 110, 98
  )
  jung_parekh <- c(
    107, 108, 106, 98, 105, 103, 110, 105, 104, 100, 96, 108,
    103, 104, 114, 114, 113, 108, 106, 99
  )

  result <- tidy_stats(mood.test(ramsay, jung_parekh))

  expect_equal(result$statistics[[1]]$value, 1.037128, tolerance = 0.05) # z
  expect_equal(result$statistics[[2]]$value, 0.2996764, tolerance = 0.05) # p
})

# quade.test() ------------------------------------------------------------

test_that("Quade test works", {
  dataFreq <- matrix(
    nrow = 7,
    byrow = TRUE,
    data = c(
      5, 4, 7, 10, 12,
      1, 3, 1, 0, 2,
      16, 12, 22, 22, 35,
      5, 4, 3, 5, 4,
      10, 9, 7, 13, 10,
      19, 18, 28, 37, 58,
      10, 7, 6, 8, 7
    ),
    dimnames = list(Store = as.character(1:7), Brand = LETTERS[1:5])
  )

  result <- tidy_stats(quade.test(dataFreq))

  expect_equal(result$statistics[[1]]$value, 3.829252,  tolerance = 1e-4) # F
  expect_equal(result$statistics[[2]]$value, 4,          tolerance = 1e-6) # df numerator
  expect_equal(result$statistics[[3]]$value, 24,         tolerance = 1e-6) # df denominator
  expect_equal(result$statistics[[4]]$value, 0.01518902, tolerance = 1e-4) # p
})

# bartlett.test() ---------------------------------------------------------

test_that("Bartlett test of homogeneity of variances works", {
  result <- tidy_stats(bartlett.test(InsectSprays$count, InsectSprays$spray))

  expect_equal(result$statistics[[1]]$value, 25.95983,    tolerance = 1e-4) # K2
  expect_equal(result$statistics[[2]]$value, 5,            tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 9.085122e-05, tolerance = 1e-4) # p
})

# fligner.test() ----------------------------------------------------------

test_that("Fligner-Killeen test of homogeneity of variances works", {
  result <- tidy_stats(fligner.test(InsectSprays$count, InsectSprays$spray))

  expect_equal(result$statistics[[1]]$value, 14.48278,  tolerance = 1e-4) # statistic
  expect_equal(result$statistics[[2]]$value, 5,          tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 0.01281678, tolerance = 1e-4) # p
})

# poisson.test() ----------------------------------------------------------

test_that("Exact Poisson test works", {
  result <- tidy_stats(poisson.test(137, 24.19893))

  expect_equal(result$statistics[[1]]$value, 5.661407,    tolerance = 1e-4) # lambda
  expect_equal(result$statistics[[2]]$value, 137,          tolerance = 1e-6) # x
  expect_equal(result$statistics[[4]]$value, 2.845227e-56, tolerance = 1e-4) # p
})

test_that("Comparison of Poisson rates", {
  result <- tidy_stats(poisson.test(c(11, 6 + 8 + 7), c(800, 1083 + 1050 + 878)))

  expect_equal(result$statistics[[1]]$value, 1.971488,  tolerance = 1e-4) # rate ratio
  expect_equal(result$statistics[[2]]$value, 11,         tolerance = 1e-6) # x
  expect_equal(result$statistics[[4]]$value, 0.07966863, tolerance = 1e-4) # p
})

# shapiro.test() ----------------------------------------------------------

test_that("Shapiro-Wilk normality test works", {
  set.seed(1)

  result <- tidy_stats(shapiro.test(runif(100, min = 2, max = 4)))

  expect_equal(result$statistics[[1]]$value, 0.9593461,  tolerance = 1e-4) # W
  expect_equal(result$statistics[[2]]$value, 0.003612854, tolerance = 1e-4) # p
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

  result <- tidy_stats(friedman.test(rounding_times))

  expect_equal(result$statistics[[1]]$value, 11.14286,   tolerance = 1e-4) # chi-squared
  expect_equal(result$statistics[[2]]$value, 2,           tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 0.003805041, tolerance = 1e-4) # p
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

  result <- tidy_stats(mantelhaen.test(Satisfaction))

  expect_equal(result$statistics[[1]]$value, 10.20009,  tolerance = 1e-4) # X2
  expect_equal(result$statistics[[2]]$value, 9,          tolerance = 1e-6) # df
  expect_equal(result$statistics[[3]]$value, 0.3345312, tolerance = 1e-4) # p
})

test_that("Mantel-Haenszel test (with continuity correction) works", {
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
      Penicillin.Level = c("1/8", "1/4", "1/2", "1", "4")
    )
  )

  result <- tidy_stats(mantelhaen.test(Rabbits))

  expect_equal(result$statistics[[1]]$value, 7,          tolerance = 1e-6) # OR
  expect_equal(result$statistics[[2]]$value, 3.928571,  tolerance = 1e-4) # X2
  expect_equal(result$statistics[[3]]$value, 1,          tolerance = 1e-6) # df
  expect_equal(result$statistics[[4]]$value, 0.04747226, tolerance = 1e-4) # p
})

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
      Penicillin.Level = c("1/8", "1/4", "1/2", "1", "4")
    )
  )

  result <- tidy_stats(mantelhaen.test(Rabbits, exact = TRUE))

  expect_equal(result$statistics[[1]]$value, 10.36102,  tolerance = 1e-4) # OR
  expect_equal(result$statistics[[2]]$value, 16,         tolerance = 1e-6) # M
  expect_equal(result$statistics[[3]]$value, 0.0399449, tolerance = 1e-4) # p
})
