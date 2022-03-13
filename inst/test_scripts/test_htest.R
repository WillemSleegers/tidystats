
# Todo
# ?stats::PP.test()
# ?stats::Box.test()
# ?stats::mood.test()
# ?stats::binom.test()
# ?stats::quade.test()
# ?stats::ansari.test()
# ?stats::bartlett.test()
# ?stats::fligner.test()
# ?stats::kruskal.test()
# ?stats::mauchly.test()
# ?stats::mcnemar.test()
# ?stats::poisson.test()
# ?stats::shapiro.test()
# ?stats::friedman.test()
# ?stats::mantelhaen.test()
# ?stats::prop.trend.test()
# ?stats::pairwise.t.test()
# ?stats::pairwise.prop.test()
# ?stats::pairwise.wilcox.test()

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

t_test_one_sample
t_test_two_sample
t_test_welch
t_test_paired

# Tidy stats
temp <- tidy_stats(t_test_one_sample)
temp <- tidy_stats(t_test_two_sample)
temp <- tidy_stats(t_test_welch)
temp <- tidy_stats(t_test_paired)

# Add stats
results <- results %>%
  add_stats(t_test_one_sample) %>%
  add_stats(t_test_two_sample) %>%
  add_stats(t_test_welch) %>%
  add_stats(t_test_paired)

# cor.test() --------------------------------------------------------------

# Get data
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c(2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

# Run analyses
correlation_pearson <- cor.test(x, y, method = "pearson")
correlation_kendall <- cor.test(x, y, method = "kendall")
correlation_spearman <- cor.test(x, y, method = "spearman")

correlation_pearson
correlation_kendall
correlation_spearman

# Tidy stats
temp <- tidy_stats(correlation_pearson)
temp <- tidy_stats(correlation_kendall)
temp <- tidy_stats(correlation_spearman)

# Add stats
results <- results %>%
  add_stats(correlation_pearson) %>%
  add_stats(correlation_kendall) %>%
  add_stats(correlation_spearman)

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

chi_squared
chi_squared_yates
chi_squared_prob

# Tidy results
temp <- tidy_stats(chi_squared)
temp <- tidy_stats(chi_squared_yates)
temp <- tidy_stats(chi_squared_prob)

# Add stats
results <- results %>%
  add_stats(chi_squared) %>%
  add_stats(chi_squared_yates) %>%
  add_stats(chi_squared_prob)

# prop.test() -------------------------------------------------------------

# Set seed
set.seed(1)

# Get data
heads <- rbinom(1, size = 100, prob = .5)
smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )

# Run analyses
prop_test <- prop.test(heads, 100)
prop_test_correct <- prop.test(heads, 100, correct = FALSE)

prop_test_smokers <- prop.test(smokers, patients)

prop_test
prop_test_correct
prop_test_smokers

# Tidy stats
temp <- tidy_stats(prop_test)
temp <- tidy_stats(prop_test_correct)
temp <- tidy_stats(prop_test_smokers)

# Add stats
results <- results %>%
  add_stats(prop_test) %>%
  add_stats(prop_test_correct) %>%
  add_stats(prop_test_smokers)

# wilcox.test() -----------------------------------------------------------

# Get data
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

# Run analyses
wilcoxon_signed_rank <- wilcox.test(x, y, paired = TRUE,
  alternative = "greater")
wilcoxon_rank_sum_continuity <- wilcox.test(Ozone ~ Month, data = airquality,
  subset = Month %in% c(5, 8))

wilcoxon_signed_rank
wilcoxon_rank_sum_continuity

# Get data
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

# Run analyses
wilcoxon_rank_sum <- wilcox.test(x, y, alternative = "greater", exact = FALSE,
  correct = FALSE)
wilcoxon_rank_sum_conf <- wilcox.test(x, y, conf.int = TRUE, conf.level = .9)

wilcoxon_rank_sum
wilcoxon_rank_sum_conf

# Tidy stats
temp <- tidy_stats(wilcoxon_signed_rank)
temp <- tidy_stats(wilcoxon_rank_sum_continuity)
temp <- tidy_stats(wilcoxon_rank_sum)
temp <- tidy_stats(wilcoxon_rank_sum_conf)

# Add stats
results <- results %>%
  add_stats(wilcoxon_signed_rank) %>%
  add_stats(wilcoxon_rank_sum_continuity) %>%
  add_stats(wilcoxon_rank_sum) %>%
  add_stats(wilcoxon_rank_sum_conf)

# kruskal.test() ----------------------------------------------------------

# Get data
x <- c(2.9, 3.0, 2.5, 2.6, 3.2)
y <- c(3.8, 2.7, 4.0, 2.4)     
z <- c(2.8, 3.4, 3.7, 2.2, 2.0)

# Run analyses
kruskal <- kruskal.test(list(x, y, z))
kruskal_formula <- kruskal.test(Ozone ~ Month, data = airquality)

kruskal
kruskal_formula 

# Tidy stats
temp <- tidy_stats(kruskal)
temp <- tidy_stats(kruskal_formula)

# Add stats
results <- results %>%
  add_stats(kruskal) %>%
  add_stats(kruskal_formula)

# fisher.test() -----------------------------------------------------------

# Get data
TeaTasting <- matrix(c(3, 1, 1, 3), nrow = 2,
  dimnames = list(Guess = c("Milk", "Tea"), Truth = c("Milk", "Tea")))

Convictions <- matrix(c(2, 10, 15, 3), nrow = 2, dimnames =
    list(c("Dizygotic", "Monozygotic"), c("Convicted", "Not convicted")))

Job <- matrix(c(1, 2, 1, 0, 3, 3, 6, 1, 10, 10, 14, 9, 6, 7, 12, 11), 4, 4,
  dimnames = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
    satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS")))

MP6 <- rbind(
  c(1, 2, 2, 1, 1, 0, 1),
  c(2, 0, 0, 2, 3, 0, 0),
  c(0, 1, 1, 1, 2, 7, 3),
  c(1, 1, 2, 0, 0, 0, 1),
  c(0, 1, 1, 1, 1, 0, 0)
)

# Run analyses
set.seed(2015)

fisher_test <- fisher.test(TeaTasting, alternative = "greater")
fisher_test_no_CI <- fisher.test(Convictions, conf.int = FALSE)
fisher_test_r_by_c <- fisher.test(Job)
fisher_test_simulated_p <- fisher.test(Job, simulate.p.value = TRUE, B = 1e5)
fisher_test_hybrid <- fisher.test(MP6, hybrid = TRUE)

fisher_test
fisher_test_no_CI
fisher_test_r_by_c
fisher_test_simulated_p
fisher_test_hybrid

# Tidy stats
temp <- tidy_stats(fisher_test)
temp <- tidy_stats(fisher_test_no_CI)
temp <- tidy_stats(fisher_test_r_by_c)
temp <- tidy_stats(fisher_test_simulated_p)
temp <- tidy_stats(fisher_test_hybrid)

# Add stats
results <- results %>%
  add_stats(fisher_test) %>%
  add_stats(fisher_test_no_CI) %>%
  add_stats(fisher_test_r_by_c) %>%
  add_stats(fisher_test_simulated_p) %>%
  add_stats(fisher_test_hybrid)

# ks.test() ---------------------------------------------------------------

# Seed
set.seed(1)

# Get data
x <- rnorm(50)
y <- runif(30)

# Run analyses
ks_test <- ks.test(x, y)
ks_test_two <- ks.test(x + 2, "pgamma", 3, 2)
ks_test_exact <- ks.test(x + 2, "pgamma", 3, 2, exact = FALSE)
ks_test_greater <- ks.test(x + 2, "pgamma", 3, 2, alternative = "greater")

ks_test
ks_test_two
ks_test_exact
ks_test_greater

# Tidy stats
temp <- tidy_stats(ks_test)
temp <- tidy_stats(ks_test_two)
temp <- tidy_stats(ks_test_exact)
temp <- tidy_stats(ks_test_greater)

# Add stats
results <- results %>%
  add_stats(ks_test) %>%
  add_stats(ks_test_two) %>%
  add_stats(ks_test_exact) %>%
  add_stats(ks_test_greater) 

# oneway.test() -----------------------------------------------------------

# Run analyses
oneway_test <- oneway.test(extra ~ group, data = sleep)
oneway_test_equal_var <- oneway.test(extra ~ group, data = sleep, 
  var.equal = TRUE)

oneway_test
oneway_test_equal_var

# Tidy stats
temp <- tidy_stats(oneway_test)
temp <- tidy_stats(oneway_test_equal_var)

# Add stats
results <- results %>%
  add_stats(oneway_test) %>%
  add_stats(oneway_test_equal_var)

# var.test() --------------------------------------------------------------

# Set seed
set.seed(1)

# Get data
x <- rnorm(50, mean = 0, sd = 2)
y <- rnorm(30, mean = 1, sd = 1)

# Run analysis
var_test <- var.test(x, y)

var_test

# Tidy stats
temp <- tidy_stats(var_test)

# Add stats
results <- add_stats(results, var_test)

# write_stats() -----------------------------------------------------------

write_stats(results, "inst/test_data/htest.json")
