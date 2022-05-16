
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
results <- list()

# t.test() ----------------------------------------------------------------

# Run analyses
t_test_one_sample <- t.test(extra ~ 1, data = sleep)
t_test_two_sample <- t.test(extra ~ group, data = sleep, var.equal = TRUE)
t_test_welch <- t.test(extra ~ group, data = sleep)
t_test_paired <- t.test(extra ~ group, data = sleep, paired = TRUE)

# Add stats
results <- results %>%
  add_stats(t_test_one_sample) %>%
  add_stats(t_test_two_sample) %>%
  add_stats(t_test_welch) %>%
  add_stats(t_test_paired)

# Inspect output
t_test_one_sample
t_test_two_sample
t_test_welch
t_test_paired

# cor.test() --------------------------------------------------------------

# Get data
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)

# Run analyses
correlation_pearson <- cor.test(x, y, method = "pearson")
correlation_kendall <- cor.test(x, y, method = "kendall")
correlation_spearman <- cor.test(x, y, method = "spearman")

# Add stats
results <- results %>%
  add_stats(correlation_pearson) %>%
  add_stats(correlation_kendall) %>%
  add_stats(correlation_spearman)

# Inspect output
correlation_pearson
correlation_kendall
correlation_spearman

# chisq.test() ------------------------------------------------------------

# Get data
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"), party = c("Democrat","Independent",
  "Republican"))
x <- matrix(c(12, 5, 7, 7), ncol = 2)
y <- c(A = 20, B = 15, C = 25)

# Run chi-squares
chi_squared <- chisq.test(M)
chi_squared_yates <- chisq.test(x)
chi_squared_prob <- chisq.test(y)

# Add stats
results <- results %>%
  add_stats(chi_squared) %>%
  add_stats(chi_squared_yates) %>%
  add_stats(chi_squared_prob)

# Inspect output
chi_squared
chi_squared_yates
chi_squared_prob

# prop.test() -------------------------------------------------------------

# Set seed
set.seed(1)

# Get data
heads <- rbinom(1, size = 100, prob = .5)
smokers  <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)

# Run analyses
prop_test <- prop.test(heads, 100)
prop_test_correct <- prop.test(heads, 100, correct = FALSE)
prop_test_smokers <- prop.test(smokers, patients)

# Add stats
results <- results %>%
  add_stats(prop_test) %>%
  add_stats(prop_test_correct) %>%
  add_stats(prop_test_smokers)

# Inspect output
prop_test
prop_test_correct
prop_test_smokers


# prop.trend.test()  --------------------------------------------------------------

# Get data
smokers  <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)
prop.test(smokers, patients)

# Run analysis
prop_trend_test = prop.trend.test(smokers, patients)
prop_trend_test_scores = prop.trend.test(smokers, patients, c(0,0,0,1))

# Add stats
results <- results %>%
  add_stats(prop_trend_test) %>%
  add_stats(prop_trend_test_scores)

# Inspect output
prop_trend_test
prop_trend_test_scores


# wilcox.test() -----------------------------------------------------------

# Get data
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

# Run analyses
wilcoxon_signed_rank <- wilcox.test(x, y, paired = TRUE,
  alternative = "greater")
wilcoxon_rank_sum_continuity <- wilcox.test(Ozone ~ Month, data = airquality,
  subset = Month %in% c(5, 8))

# Get data
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

# Run analyses
wilcoxon_rank_sum <- wilcox.test(x, y, alternative = "greater", exact = FALSE,
  correct = FALSE)
wilcoxon_rank_sum_conf <- wilcox.test(x, y, conf.int = TRUE, conf.level = .9)

# Add stats
results <- results %>%
  add_stats(wilcoxon_signed_rank) %>%
  add_stats(wilcoxon_rank_sum_continuity) %>%
  add_stats(wilcoxon_rank_sum) %>%
  add_stats(wilcoxon_rank_sum_conf)

# Inspect output
wilcoxon_signed_rank
wilcoxon_rank_sum_continuity
wilcoxon_rank_sum
wilcoxon_rank_sum_conf

# kruskal.test() ----------------------------------------------------------

# Get data
x <- c(2.9, 3.0, 2.5, 2.6, 3.2)
y <- c(3.8, 2.7, 4.0, 2.4)
z <- c(2.8, 3.4, 3.7, 2.2, 2.0)

# Run analyses
kruskal <- kruskal.test(list(x, y, z))
kruskal_formula <- kruskal.test(Ozone ~ Month, data = airquality)

# Add stats
results <- results %>%
  add_stats(kruskal) %>%
  add_stats(kruskal_formula)

# Inspect output
kruskal
kruskal_formula

# fisher.test() -----------------------------------------------------------

# Set seed
set.seed(2015)

# Get data
TeaTasting <- matrix(
  data = c(3, 1, 1, 3),
  nrow = 2,
  dimnames = list(Guess = c("Milk", "Tea"), Truth = c("Milk", "Tea"))
)

Convictions <- matrix(
  data = c(2, 10, 15, 3),
  nrow = 2,
  dimnames = list(
    c("Dizygotic", "Monozygotic"),
    c("Convicted", "Not convicted")
  )
)

Job <- matrix(
  data = c(1, 2, 1, 0, 3, 3, 6, 1, 10, 10, 14, 9, 6, 7, 12, 11),
  nrow = 4,
  ncol = 4,
  dimnames = list(
    income = c("< 15k", "15-25k", "25-40k", "> 40k"),
    satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS")
  )
)

MP6 <- rbind(
  c(1, 2, 2, 1, 1, 0, 1),
  c(2, 0, 0, 2, 3, 0, 0),
  c(0, 1, 1, 1, 2, 7, 3),
  c(1, 1, 2, 0, 0, 0, 1),
  c(0, 1, 1, 1, 1, 0, 0)
)

# Run analyses
fisher_test <- fisher.test(TeaTasting, alternative = "greater")
fisher_test_no_CI <- fisher.test(Convictions, conf.int = FALSE)
fisher_test_r_by_c <- fisher.test(Job)
fisher_test_simulated_p <- fisher.test(Job, simulate.p.value = TRUE, B = 1e5)
fisher_test_hybrid <- fisher.test(MP6, hybrid = TRUE)

# Add stats
results <- results %>%
  add_stats(fisher_test) %>%
  add_stats(fisher_test_no_CI) %>%
  add_stats(fisher_test_r_by_c) %>%
  add_stats(fisher_test_simulated_p) %>%
  add_stats(fisher_test_hybrid)

# Inspect output
fisher_test
fisher_test_no_CI
fisher_test_r_by_c
fisher_test_simulated_p
fisher_test_hybrid

# ks.test() ---------------------------------------------------------------

# Seed
set.seed(1)

# Get data
x <- rnorm(50)
y <- runif(30)

# Run analyses
ks_test_two <- ks.test(x, y)
ks_test_one <- ks.test(x + 2, "pgamma", 3, 2)
ks_test_inexact <- ks.test(x + 2, "pgamma", 3, 2, exact = FALSE)
ks_test_greater <- ks.test(x + 2, "pgamma", 3, 2, alternative = "greater")

# Add stats
results <- results %>%
  add_stats(ks_test_two) %>%
  add_stats(ks_test_one) %>%
  add_stats(ks_test_inexact) %>%
  add_stats(ks_test_greater)

# Inspect output
ks_test_two
ks_test_one
ks_test_inexact
ks_test_greater

# oneway.test() -----------------------------------------------------------

# Run analyses
oneway_test <- oneway.test(extra ~ group, data = sleep)
oneway_test_equal_var <- oneway.test(extra ~ group, data = sleep,
  var.equal = TRUE)

# Add stats
results <- results %>%
  add_stats(oneway_test) %>%
  add_stats(oneway_test_equal_var)

# Inspect output
oneway_test
oneway_test_equal_var

# var.test() --------------------------------------------------------------

# Set seed
set.seed(1)

# Get data
x <- rnorm(50, mean = 0, sd = 2)
y <- rnorm(30, mean = 1, sd = 1)

# Run analysis
var_test <- var.test(x, y)

# Add stats
results <- add_stats(results, var_test)

# Inspect output
var_test


# mauchly.test() --------------------------------------------------------------

# Get data: brings in the mlmfit and reacttime objects
utils::example(SSD)

# Run analyses

# traditional test of intrasubj. contrasts
mauchly_test = mauchly.test(mlmfit, X = ~ 1)
# tests using intra-subject 3x2 design
idata <- data.frame(deg = gl(3, 1, 6, labels = c(0, 4, 8)),
noise = gl(2, 3, 6, labels = c("A", "P")))
mauchly_test_orthogonal = mauchly.test(mlmfit, X = ~ deg + noise, idata = idata)
mauchly_test_spanned = mauchly.test(mlmfit,
  M = ~ deg + noise,
  X = ~ noise,
  idata = idata)

# Add stats
results <- results %>%
  add_stats(mauchly_test) %>%
  add_stats(mauchly_test_orthogonal) %>%
  add_stats(mauchly_test_spanned)

# Inspect output
mauchly_test
mauchly_test_orthogonal
mauchly_test_spanned


# mcnemar.test()  --------------------------------------------------------------

# Get data
Performance <-
  matrix(
    c(794, 86, 150, 570),
    nrow = 2,
    dimnames = list(
      "1st Survey" = c("Approve", "Disapprove"),
      "2nd Survey" = c("Approve", "Disapprove")
    )
  )

# Run analysis
mcnemar_test <- mcnemar.test(Performance)
mcnemar_test_nocorrect <- mcnemar.test(Performance, correct = FALSE)

# Add stats
results <- results %>%
  add_stats(mcnemar_test) %>%
  add_stats(mcnemar_test_nocorrect)

# Inspect output
mcnemar_test
mcnemar_test_nocorrect


# binom.test()  --------------------------------------------------------------

# Run analysis
binom_test <- binom.test(c(682, 243))
binom_test_params <-
  binom.test(c(682, 243), p = 3 / 4, alternative = "less")

# Add stats
results <- results %>%
  add_stats(binom_test) %>%
  add_stats(binom_test_params)

# Inspect output
binom_test
binom_test_params

# PP.test()  --------------------------------------------------------------

# Get data
set.seed(1)
x <- rnorm(1000)
y <- cumsum(x) # has unit root

# Run analysis
pp_test = PP.test(x)
pp_test_long = PP.test(y, lshort = FALSE)

# Add stats
results <- results %>%
  add_stats(pp_test) %>%
  add_stats(pp_test_long)

# Inspect output
pp_test
pp_test_long


# Box.test()  --------------------------------------------------------------

# Get data
set.seed(1)
x <- rnorm (100)

# Run analysis
box_test <- Box.test(x, lag = 1)
box_test_ljung <- Box.test (x, lag = 2, type = "Ljung")

# Add stats
results <- results %>%
  add_stats(box_test) %>%
  add_stats(box_test_ljung)

# Inspect output
box_test
box_test_ljung


# ansari.test()  --------------------------------------------------------------

# Get data
ramsay <- c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99,
  101, 96, 97, 102, 107, 113, 116, 113, 110, 98)
jung.parekh <- c(107, 108, 106, 98, 105, 103, 110, 105, 104,
  100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99)

# Run analysis
ansari_test = ansari.test(ramsay, jung.parekh)
set.seed(1)
ansari_test_ci = ansari.test(rnorm(100), rnorm(100, 0, 2), conf.int = TRUE)

# Add stats
results <- results %>%
  add_stats(ansari_test) %>%
  add_stats(ansari_test_ci)

# Inspect output
ansari_test
ansari_test_ci

# mood.test()  --------------------------------------------------------------

# (Data same as for ansari.test)

# Run analysis
mood_test <- mood.test(ramsay, jung.parekh)

# Add stats
results <- add_stats(results, mood_test)

# Inspect output
mood_test


# quade.test()  --------------------------------------------------------------

# Get data
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

# Run analysis
quade_test <- quade.test(dataFreq)

# Add stats
results <- add_stats(results, quade_test)

# Inspect output
quade_test

# bartlett.test()  --------------------------------------------------------------

# Run analysis
bartlett_test = bartlett.test(InsectSprays$count, InsectSprays$spray)

# Add stats
results <- add_stats(results, bartlett_test)

# Inspect output
bartlett_test

# fligner.test()  --------------------------------------------------------------

# Run analysis
fligner_test = fligner.test(InsectSprays$count, InsectSprays$spray)

# Add stats
results <- add_stats(results, fligner_test)

# Inspect output
fligner_test

# poisson.test()  --------------------------------------------------------------

# Run analysis
poisson_test = poisson.test(137, 24.19893)
poisson_test_comparison = poisson.test(c(11, 6+8+7), c(800, 1083+1050+878))

add_stats(results, poisson_test) 
add_stats(results, poisson_test_comparison)

# Add stats
results <- results %>%
  add_stats(poisson_test) %>%
  add_stats(poisson_test_comparison)

# Inspect output
poisson_test
poisson_test_comparison


# shapiro.test()  --------------------------------------------------------------

set.seed(1)

# Run analysis
shapiro_test = shapiro.test(runif(100, min = 2, max = 4))

# Add stats
results <- add_stats(results, shapiro_test)

# Inspect output
shapiro_test

# friedman.test()  --------------------------------------------------------------

# Get data
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

# Run analysis
friedman_test = friedman.test(RoundingTimes)

# Add stats
results <- add_stats(results, friedman_test)

# Inspect output
friedman_test


# mantelhaen.test()  --------------------------------------------------------------

# Get data
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

# Run analysis

## Cochran-Mantel-Haenszel (without 2 by 2)
mantelhaen_test = mantelhaen.test(Satisfaction)
## Classical Mantel-Haenszel test
mantelhaen_test_2by2 = mantelhaen.test(Rabbits)
## Exact conditional test
mantelhaen_test_2by2_exact = mantelhaen.test(Rabbits, exact = TRUE)

# Add stats
# results <- add_stats(results, SOME_test)
results <- results %>%
  add_stats(mantelhaen_test) %>%
  add_stats(mantelhaen_test_2by2) %>%
  add_stats(mantelhaen_test_2by2_exact)

# Inspect output
mantelhaen_test
mantelhaen_test_2by2
mantelhaen_test_2by2_exact


# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/htest.json")
