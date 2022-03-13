
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# add_stats() -------------------------------------------------------------

# Conduct three different analyses
# t-test:
sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)

# lm:
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm_D9 <- lm(weight ~ group)

# ANOVA:
npk_aov <- aov(yield ~ block + N*P*K, npk)

# Create an empty list
results <- list()

# Add the analyses to the empty list
results <- results %>%
  add_stats(sleep_test, type = "primary") %>%
  add_stats(lm_D9, preregistered = FALSE) %>%
  add_stats(npk_aov, notes = "An ANOVA example")

# write_stats() -----------------------------------------------------------

write_stats(results, "inst/results.json")

# tidy_stats_to_data_frame ------------------------------------------------

df <- tidy_stats_to_data_frame(results)
write_csv(df, "inst/results_df.csv")

# Analysis: describe_data() -------------------------------------------

results <- list()

# Run analyses
single_var <- describe_data(quote_source, response)

single_var_w_group <- quote_source %>%
  group_by(source) %>%
  describe_data(response)

multiple_var <- describe_data(quote_source, response, age)

single_var_w_groups <- quote_source %>%
  group_by(source, sex) %>%
  describe_data(response)

single_var_w_groups_wo_na <- quote_source %>%
  group_by(source, sex) %>%
  describe_data(response, na.rm = FALSE)

multiple_var_w_group <- quote_source %>%
  group_by(source) %>%
  describe_data(response, age)

single_var_subset <- describe_data(quote_source, response, short = TRUE)

single_var
single_var_subset
single_var_w_group
single_var_w_groups
single_var_w_groups_wo_na
multiple_var
multiple_var_w_group

# Tidy stats
temp <- tidy_stats(single_var)
temp <- tidy_stats(single_var_subset)
temp <- tidy_stats(single_var_w_group)
temp <- tidy_stats(single_var_w_groups)
temp <- tidy_stats(single_var_w_groups_wo_na)
temp <- tidy_stats(multiple_var)
temp <- tidy_stats(multiple_var_w_group)

# Add stats
results <- results %>%
  add_stats(single_var) %>%
  add_stats(single_var_w_group) %>%
  add_stats(single_var_w_groups) %>%
  add_stats(single_var_subset)

write_stats(results, "inst/test_data/describe_data.json")

# Analysis: count_data() --------------------------------------------------

results <- list()

# Run analyses
no_group <- count_data(quote_source)
single_group <- count_data(quote_source, source)
two_groups <- count_data(quote_source, source, sex)

grouped_group <- quote_source %>%
  group_by(source) %>%
  count_data(sex)

grouped_group_na_rm <- quote_source %>%
  group_by(source) %>%
  count_data(sex, na.rm = TRUE)

no_group
single_group
two_groups
grouped_group
grouped_group_na_rm

# Tidy stats
temp <- tidy_stats(no_group)
temp <- tidy_stats(single_group)
temp <- tidy_stats(two_groups)
temp <- tidy_stats(grouped_group)
temp <- tidy_stats(grouped_group_na_rm)

# Add stats
results <- results %>%
  add_stats(no_group) %>%
  add_stats(single_group) %>%
  add_stats(two_groups) %>%
  add_stats(grouped_group) %>%
  add_stats(grouped_group_na_rm) 

write_stats(results, "inst/test_data/count_data.json")

# Analysis: generic test --------------------------------------------------

results <- list()

# Create generic tests
generic_ICs <- list(
  statistics = list(
    BIC = 21.21,
    AIC = 478.21
  )
)

generic_CIs <- list(
  statistics = list(
    CI = list(
      CI_level = .95,
      CI_upper = .64,
      CI_lower = .32
    )
  )
)

# Add it to the list
results <- results %>%
    add_stats(generic_ICs, 
      notes = "Wagenmakers (2007) method for calculating Bayes factors") %>%
    add_stats(generic_CIs, notes = "Just some random CIs")

write_stats(results, "inst/test_data/generic.json")


# class: confint() --------------------------------------------------------

# Example 1
fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)

CI_fit <- confint(fit)
CI_fit

CI_fit_wt <- confint(fit, "wt")
CI_fit_wt

# Example 2
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9); treatment <- gl(3, 3)
glm_D93 <- glm(counts ~ outcome + treatment, family = poisson())

CI_glm_D93_MASS <- confint(glm_D93) # needs MASS to be installed
CI_glm_D93_MASS
CI_glm_D93_default <- confint.default(glm_D93)  # based on asymptotic normality
CI_glm_D93_default

# Tidy stats
temp <- tidy_stats(CI_fit)
temp <- tidy_stats(CI_fit_wt)
temp <- tidy_stats(CI_glm_D93_MASS)
temp <- tidy_stats(CI_glm_D93_default)

# Add stats
results <- list()

results <- results %>%
  add_stats(CI_fit, class = "confint") %>%
  add_stats(CI_fit_wt, class = "confint") %>%
  add_stats(CI_glm_D93_MASS, class = "confint") %>%
  add_stats(CI_glm_D93_default, class = "confint")

# Save stats
write_stats(results, "inst/test_data/confint.json")
