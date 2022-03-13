
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Analysis: htest ---------------------------------------------------------

results <- list()

# Run t-tests
t_test_one_sample <- t.test(extra ~ 1, data = sleep)
t_test_two_sample <- t.test(extra ~ group, data = sleep, var.equal = TRUE)
t_test_welch <- t.test(extra ~ group, data = sleep)
t_test_paired <- t.test(extra ~ group, data = sleep, paired = TRUE)

t_test_one_sample
t_test_two_sample
t_test_welch
t_test_paired

# Tidy results
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

# Run correlations
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c(2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

correlation_pearson <- cor.test(x, y, method = "pearson")
correlation_kendall <- cor.test(x, y, method = "kendall")
correlation_spearman <- cor.test(x, y, method = "spearman")

correlation_pearson
correlation_kendall
correlation_spearman

# Tidy results
temp <- tidy_stats(correlation_pearson)
temp <- tidy_stats(correlation_kendall)
temp <- tidy_stats(correlation_spearman)

# Add stats
results <- results %>%
  add_stats(correlation_pearson) %>%
  add_stats(correlation_kendall) %>%
  add_stats(correlation_spearman)

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

# Get data
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

# Run wilcox tests
wilcoxon_signed_rank <- wilcox.test(x, y, paired = TRUE,
  alternative = "greater")

wilcoxon_rank_sum_continuity <- wilcox.test(Ozone ~ Month, data = airquality,
  subset = Month %in% c(5, 8))

wilcoxon_signed_rank
wilcoxon_rank_sum_continuity

x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

wilcoxon_rank_sum <- wilcox.test(x, y, alternative = "greater", exact = FALSE,
  correct = FALSE)
wilcoxon_rank_sum_conf <- wilcox.test(x, y, conf.int = TRUE, conf.level = .9)

wilcoxon_rank_sum
wilcoxon_rank_sum_conf

# Tidy results
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

# Run Fisher tests
fisher_test <- fisher.test(TeaTasting, alternative = "greater")
fisher_test_no_CI <- fisher.test(Convictions, conf.int = FALSE)
fisher_test_r_by_c <- fisher.test(Job)

set.seed(2015)
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

# Oneway test
# Run oneway tests
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

write_stats(results, "inst/test_data/htest.json")

# Analysis: lm() ----------------------------------------------------------

results <- list()

# Get data
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)

# Run regressions
lm <- lm(weight ~ group)
lm_wo_intercept <- lm(weight ~ group - 1) # omitting intercept

summary(lm)
summary(lm_wo_intercept)

# Tidy results
temp <- tidy_stats(lm)
temp <- tidy_stats(lm_wo_intercept)

# Add stats
results <- results %>%
  add_stats(lm) %>%
  add_stats(lm_wo_intercept)

write_stats(results, "inst/test_data/lm.json")

# Analysis: glm() ---------------------------------------------------------

results <- list()

# Example 1: Dobson (1990) Page 93: Randomized Controlled Trial
# Get data
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
d.AD <- data.frame(treatment, outcome, counts)

# Run model
glm_poisson <- glm(counts ~ outcome + treatment, family = poisson())
summary(glm_poisson)

# Tidy data
temp <- tidy_stats(glm_poisson)

# Example 2: Venables & Ripley (2002, p.189)
# Get data
anorexia <- tibble(
  Treat = c("Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", 
    "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", 
    "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", "Cont", 
    "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", 
    "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", 
    "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "CBT", "FT", 
    "FT", "FT", "FT", "FT", "FT", "FT", "FT", "FT", "FT", "FT", "FT", "FT", 
    "FT", "FT", "FT", "FT"),
  Prewt = c(80.7, 89.4, 91.8, 74.0, 78.1, 88.3, 87.3, 75.1, 80.6, 78.4, 77.6, 
    88.7, 81.3, 78.1, 70.5, 77.3, 85.2, 86.0, 84.1, 79.7, 85.5, 84.4, 79.6, 
    77.5, 72.3, 89.0, 80.5, 84.9, 81.5, 82.6, 79.9, 88.7, 94.9, 76.3, 81.0, 
    80.5, 85.0, 89.2, 81.3, 76.5, 70.0, 80.4, 83.3, 83.0, 87.7, 84.2, 86.4, 
    76.5, 80.2, 87.8, 83.3, 79.7, 84.5, 80.8, 87.4, 83.8, 83.3, 86.0, 82.5, 
    86.7, 79.6, 76.9, 94.2, 73.4, 80.5, 81.6, 82.1, 77.6, 83.5, 89.9, 86.0, 
    87.3),
  Postwt = c( 80.2, 80.1, 86.4, 86.3, 76.1, 78.1, 75.1, 86.7, 73.5, 84.6, 77.4, 
    79.5, 89.6, 81.4, 81.8, 77.3, 84.2, 75.4, 79.5, 73.0, 88.3, 84.7, 81.4, 
    81.2, 88.2, 78.8, 82.2, 85.6, 81.4, 81.9, 76.4, 103.6, 98.4, 93.4, 73.4, 
    82.1, 96.7, 95.3, 82.4, 72.5, 90.9, 71.3, 85.4, 81.6, 89.1, 83.9, 82.7, 
    75.7, 82.6, 100.4, 85.2, 83.6, 84.6, 96.2, 86.7, 95.2, 94.3, 91.5, 91.9, 
    100.3, 76.7, 76.8, 101.6, 94.9, 75.2, 77.8, 95.5, 90.7, 92.5, 93.8, 91.7, 
    98.0)
)

# Run model
glm_gaussian <- glm(Postwt ~ Prewt + Treat + offset(Prewt), data = anorexia)
summary(glm_gaussian)

# Tidy data
temp <- tidy_stats(glm_gaussian)

# Example 3: McCullagh & Nelder (1989, pp. 300-2)
# Get data
clotting <- tibble(
  u = c(5,10,15,20,30,40,60,80,100),
  lot1 = c(118,58,42,35,27,25,21,19,18),
  lot2 = c(69,35,26,21,18,16,13,12,12)
)

# Run model
glm_gamma <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
summary(glm_gamma)

glm_gamma_fs <- glm(lot2 ~ log(u) + log(u^2), data = clotting, family = Gamma)
glm_gamma_fs

# Tidy stats
temp <- tidy_stats(glm_gamma)

# Example 4: Logistic regression
# Get data
admission <- tibble(
  admit = c(0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0),
  gre = c(380, 660, 800, 640, 520, 760, 560, 400, 540, 700, 800),
  gpa = c(3.61, 3.67, 4.00, 3.19, 2.93, 3.00, 2.98, 3.08, 3.39, 3.92, 4.00),
  rank = c(3, 3, 1, 4, 4, 2, 1, 2, 3, 2, 4)
)

# Run model
glm_binomial = glm(admit ~ gre + gpa + rank, data = admission, 
  family = "binomial")
summary(glm_binomial)

# Add stats
results <- results %>%
  add_stats(glm_poisson) %>%
  add_stats(glm_gaussian) %>%
  add_stats(glm_gamma) %>%
  add_stats(glm_gamma_fs) %>%
  add_stats(glm_binomial)

write_stats(results, "inst/test_data/glm.json")

# Analysis: anova.lm() ----------------------------------------------------

results <- list()

# Perform tests
fit <- lm(sr ~ ., data = LifeCycleSavings)

fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
fit1 <- update(fit0, . ~ . + pop15)
fit2 <- update(fit1, . ~ . + pop75)
fit3 <- update(fit2, . ~ . + dpi)
fit4 <- update(fit3, . ~ . + ddpi)

anova_lm <- anova(fit)
anova_lm_fits <- anova(fit0, fit1, fit2, fit3, fit4, test = "F")
anova_lm_order <- anova(fit4, fit2, fit0, test = "F")
anova_lm_chisq <- anova(fit4, fit2, fit0, test = "Chisq")
anova_lm_cp <- anova(fit4, fit2, fit0, test = "Cp")

anova_lm
anova_lm_fits
anova_lm_order
anova_lm_chisq
anova_lm_cp

temp <- tidy_stats(anova_lm)
temp <- tidy_stats(anova_lm_fits)
temp <- tidy_stats(anova_lm_order)
temp <- tidy_stats(anova_lm_chisq)
temp <- tidy_stats(anova_lm_cp)

results <- results %>%
  add_stats(anova_lm) %>%
  add_stats(anova_lm_fits) %>%
  add_stats(anova_lm_order) %>%
  add_stats(anova_lm_chisq) %>%
  add_stats(anova_lm_cp)

write_stats(results, "inst/test_data/anova_lm.json")

# Analysis: anova.glm() ---------------------------------------------------

results <- list()

# Run models
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
d.AD <- data.frame(treatment, outcome, counts)

# Run models
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
glm.D93a <- update(glm.D93, ~treatment * outcome)

anova_glm <- anova(glm.D93)
anova_glm_cp <- anova(glm.D93, test = "Cp")
anova_glm_chisq <- anova(glm.D93, test = "Chisq")
anova_glm_rao <- anova(glm.D93, glm.D93a, test = "Rao")

anova_glm
anova_glm_cp
anova_glm_chisq
anova_glm_rao

# Tidy stats
temp <- tidy_stats(anova_glm)
temp <- tidy_stats(anova_glm_cp)
temp <- tidy_stats(anova_glm_chisq)
temp <- tidy_stats(anova_glm_rao)

# Add stats
results <- results %>%
  add_stats(anova_glm) %>%
  add_stats(anova_glm_cp) %>%
  add_stats(anova_glm_chisq) %>%
  add_stats(anova_glm_rao)

write_stats(results, "inst/test_data/anova_glm.json")

# Analysis: aov() ---------------------------------------------------------

results <- list()

# Run models
aov <- aov(yield ~ block + N * P * K, npk)
aov_order <- aov(terms(yield ~ block + N * P + K, keep.order = TRUE), npk)
aov_error <- aov(yield ~ N * P * K + Error(block), npk)

summary(aov)
summary(aov_order)
summary(aov_error)

# Tidy results
temp <- tidy_stats(aov)
temp <- tidy_stats(aov_order)
temp <- tidy_stats(aov_error)

# Add stats
results <- results %>%
  add_stats(aov) %>%
  add_stats(aov_order) %>%
  add_stats(aov_error)

write_stats(results, "inst/test_data/aov.json")
