
# Analysis: afex ----------------------------------------------------------


# Analysis: confint() -----------------------------------------------------

# Run analysis
lm_confint <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
confint_lm_95 <- confint(lm_confint, level = .95)
confint_lm_90 <- confint(lm_confint, level = .90)

counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3, 1, 9); treatment <- gl(3, 3)
glm_confint <- glm(counts ~ outcome + treatment, family = poisson())
confint_glm_profile_likelihood <- confint(glm_confint)
confint_glm_asymptotic_normality <- confint.default(glm_confint)

# Add stats
add_stats(list(), confint_lm_95, class = "confint")
add_stats(list(), confint_lm_90, class = "confint")
add_stats(list(), confint_glm_profile_likelihood, class = "confint")
add_stats(list(), confint_glm_asymptotic_normality, class = "confint")

# Add stats to model
results <- results %>%
  add_stats(lm_confint) %>%
  add_stats(confint_lm_95, class = "confint") %>%
  add_stats(confint_lm_90, class = "confint") %>%
  add_stats_to_model(confint_lm_95, identifier = "lm_confint",
    class = "confint") %>%
  add_stats(confint_glm_profile_likelihood, class = "confint") %>%
  add_stats(confint_glm_asymptotic_normality, class = "confint")


# Analysis: psych ---------------------------------------------------------

# Load package
library(psych)

# Analysis: psych’s alpha() -----------------------------------------------

# Run alpha
psych_alpha <- alpha(dplyr::select(epi, V1, V3, V8, V10, V13, V17,
  V22, V25, V27, V39, V44, V46, V49, V53, V56))

# Tidy stats
tidy_stats(psych_alpha)

# Add stats
results <- add_stats(results, psych_alpha)

# Analysis: psych’s corr.test() -------------------------------------------

# Get some data
attitude <- datasets::attitude

# Run analysis
psych_correlations <- corr.test(attitude, adjust = "none")
print(psych_correlations, short = FALSE)

# Tidy results
tidy_stats(psych_correlations)

# Add stats
results <- add_stats(results, psych_correlations)

# Analysis: psych's ICC ---------------------------------------------------

# Load data
sf <- matrix(ncol = 4, byrow = TRUE,
  c(9,  2, 5, 8,
    6,  1, 3, 2,
    8,  4, 6, 8,
    7,  1, 2, 6,
    10, 5, 6, 9,
    6,  2, 4, 7))
colnames(sf) <- paste("J", 1:4, sep = "")
rownames(sf) <- paste("S", 1:6, sep = "")

# Perform analysis
psych_ICC <- ICC(sf, lmer = FALSE)

# Tidy stats
tidy_stats(psych_ICC)

# Add stats
results <- add_stats(results, psych_ICC)


# Analysis: afex ----------------------------------------------------------

# Load afex package
library(afex)

# Load data
data(obk.long, package = "afex")

# Perform analyses
afex_aov_ez <- aov_ez("id", "value", obk.long, within = c("phase", "hour"),
  between = c("treatment", "gender"), observed = "gender")
afex_aov_car <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)),
  data = obk.long, observed = "gender")
afex_aov_4 <- aov_4(value ~ treatment * gender + (phase*hour|id),
  data = obk.long, observed = "gender")

# Tidy stats
tidy_stats(afex_aov_ez)
tidy_stats(afex_aov_car)
tidy_stats(afex_aov_4)

# Load data
data("sk2011.2")
sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])

# Perform analysis
sk_m1 <- mixed(response ~ instruction * inference * type +
    (inference * type | id), sk2_aff)
sk_m1

# Tidy stats
tidy_stats(sk_m1, args = list(summary = "lmer"))

# Analysis: emmeans -------------------------------------------------------

# Load packages
library(emmeans)

# Load data
pigs <- as_tibble(pigs)

# Perform analysis
pigs.lm1 <- lm(log(conc) ~ source + factor(percent), data = pigs)
pigs.lm1.emmeans <- emmeans(pigs.lm1, "percent")
pigs.lm1.emmeans

mtcars.1 <- lm(mpg ~ factor(cyl) + disp + I(disp^2), data = mtcars)
mtcars.1.emmeans <- emmeans(mtcars.1, "cyl")
mtcars.1.emmeans

noise.lm <- lm(noise ~ size * type * side, data = auto.noise)
noise.lm.emmeans <- emmeans(noise.lm, pairwise ~ size)
noise.lm.emmeans

emm_s.t.emmeans <- emmeans(noise.lm, pairwise ~ size | type)
emm_s.t.emmeans

noise.emm.emmeans <- emmeans(noise.lm, ~ size * side * type)
noise.emm.emmeans

# Tidy stats
tidy_stats(pigs.lm1.emmeans)
tidy_stats(mtcars.1.emmeans)
tidy_stats(noise.emm.emmeans)

# add_stats.data.frame() --------------------------------------------------

# Create a tidy data frame
x_squared_data <- data_frame(
  statistic = c("X-squared", "df", "p"),
  value = c(5.4885, 6, 0.4828),
  method = "Chi-squared test of independence"
)

# Add stats
results <- add_stats(results, x_squared_data, identifier = "x_squared")

# Create another tidy data frame
some_data <- tibble(
  term = c("group1", "group1", "group2", "group2"),
  statistic = c("t", "p", "t", "p"),
  value = c(5.4885, 0.04, 4.828, 0.06),
  method = "A test"
)

results <- add_stats(results, some_data, identifier = "some_data")

# add_stats(): default identifier -----------------------------------------

# Statistical test
add_stats(list(), t_test_one_sample)

# Statistical test with piping
t.test(cox$call_parent, alternative = "greater") %>%
  add_stats(list(), .)

# Data frame
cox_avoidance <- cox %>%
  describe_data(avoidance) %>%
  tidy_describe_data()

add_stats(list(), cox_avoidance)

# Data frame with piping
cox %>%
  describe_data(avoidance) %>%
  tidy_describe_data() %>%
  add_stats(list(), ., type = "d")


# In progress -------------------------------------------------------------

# Analysis: metafor -------------------------------------------------------

# Load package
library(metafor)

# Get data
dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg,
  data = dat.bcg)

# Run univariate meta-analyses
rma_uni <- rma(yi, vi, data = dat, method = "REML", level = 90)
rma_uni_mods <- rma(yi, vi, mods = cbind(ablat, year), data = dat,
  method = "REML")

rma_uni
rma_uni_mods

# Tidy results
tidy_stats(rma_uni)
tidy_stats(rma_uni_mods)

# Add stats
results <- results %>%
  add_stats(rma_uni) %>%
  add_stats(rma_uni_mods)

# Run multivariate meta-analyses

# Prepare data
# Change data into long format
dat.long <- to.long(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg,
  data = dat.bcg)

# Set levels of group variable
levels(dat.long$group) <- c("exp", "con")

# Set "con" to reference level
dat.long$group <- relevel(dat.long$group, ref = "con")

# Calculate log odds and corresponding sampling variances
dat.long <- escalc(measure = "PLO", xi = out1, mi = out2, data = dat.long)
dat.long$effect <- 1:nrow(dat.long)

# Bivariate random-effects model using rma.mv()
rma_mv <- rma.mv(yi, vi, random = ~ group | study/effect, struct="UN",
  data = dat.long)
rma_mv_mods <- rma.mv(yi, vi, mods = ~ group, random = ~ group | study,
  struct="UN", data=dat.long)

rma_mv
rma_mv_mods

# Tidy stats
tidy_stats(rma_mv)
tidy_stats(rma_mv_mods)

# Add stats
results <- results %>%
  add_stats(rma_mv) %>%
  add_stats(rma_mv_mods)

# Inspect resulst
inspect(results)

# Report results

report_rma(results, "rma_uni", statistic = NULL)
report_rma(results, "rma_uni", term = "intrcpt")

report(results = results, identifier = "rma_uni", term = "intrcpt")

report_rma(results, "rma_mv", group = "heterogeneity")
report_rma(results, "rma_mv", term = "intrcpt")

report("rma_uni", term = "intrcpt", results = results)
report("meta_analysis", term_nr = 1, statistic = "tau^2", results = results)
report("meta_analysis_mods", term = "ablat", results = results)

# Marino github issue example

dat.bcg

res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="RR",
  slab=paste(author, year, sep=", "), method="REML")
tidy_stats(res)
results <- list()
results <- add_stats(results, res, identifier = "marino_meta_analysis")
options(tidystats_list = results)
report("marino_meta_analysis", term = "intrcpt")
report("marino_meta_analysis", term = "(Heterogeneity)", s = "tau^2")
report("marino_meta_analysis", term_nr = 1, s = "tau^2")


# rcorr()
library(Hmisc)

# Create some data
x <- c(-2, -1, 0, 1, 2)
y <- c(4, 1, 0, 1, 4)
z <- c(1, 2, 3, 4, NA)
v <- c(1, 2, 3, 4, 5)

# Perform analysis
rcorr_correlations <- rcorr(cbind(x,y,z,v))
rcorr_correlations

rcorr_correlations$r[upper.tri(rcorr_correlations$r, diag = TRUE)] <- NA
rcorr_correlations$n[upper.tri(rcorr_correlations$n, diag = TRUE)] <- NA
rcorr_correlations$P[upper.tri(rcorr_correlations$P, diag = TRUE)] <- NA

r <- as.data.frame(rcorr_correlations$r)
n <- as.data.frame(rcorr_correlations$n)
p <- as.data.frame(rcorr_correlations$P)

r <- rownames_to_column(r, "term")
n <- rownames_to_column(n, "term")
p <- rownames_to_column(p, "term")

r$statistic <- "r"
n$statistic <- "n"
p$statistic <- "p"

output <- bind_rows(r, n) %>%
  bind_rows(p) %>%
  gather("var", "value", -term, -term_nr, -statistic) %>%
  filter(!is.na(value)) %>%
  unite(term, var, term, sep = "-") %>%
  mutate(
    term_nr = 1:nrow(.),
    method = "rcorr() correlation {Hmisc}"
  ) %>%
  select(term, term_nr, statistic, value, method)

output <- output %>%
  separate(term, into = c("var", "columns"), sep = "-") %>%
  filter(statistic == "r") %>%
  mutate(term_nr = floor(term_nr / 4)) %>%
  spread(columns, value)

if (TRUE) {
  output <- arrange(output, desc(term_nr))
}

output <- select(output, -term_nr, -statistic, -method)
output <- select(output, c("var",
  names(sort(colSums(is.na(select(output, -var))),
    decreasing = T))))

# Analysis: anova()
anova(model3_1)

anova(model3_1, model3_2)

anova(model5_1)

anova(model5_1, model5_2)


# MANOVA

data <- iris

model7_1 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
  data = iris), test = "Roy")
model7_2 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
  data = iris), test = "")
model7_3 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
  data = iris), test = "Roy")

model7_4 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species *
    Petal.Width , data = iris), test = "Roy")

# tidyversity
# Install and load tidyversity
# install_github("mkearney/tidyversity")
library(tidyversity)

# Ordinary Least Squares (OLS)
TVM_1 <- tidy_regression(polcom, follow_trump ~ news_1 + ambiv_sexism_1)
tidy_summary(TVM_1)

# Logistic (dichotomous)
polcom %>%
  tidy_regression(follow_trump ~ news_1 + ambiv_sexism_1, type = "logistic") %>%
  tidy_summary()

# Poisson (count)
polcom %>%
  dplyr::mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "poisson") %>%
  tidy_summary()

# Negative binomial (overdispersed)
polcom %>%
  dplyr::mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "negbinom") %>%
  tidy_summary()

# Robust and quasi- models
polcom %>%
  dplyr::mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "quasipoisson",
    robust = TRUE) %>%
  tidy_summary()

# ANOVA
polcom %>%
  dplyr::mutate(sex = ifelse(sex == 1, "Male", "Female"),
    vote_choice = dplyr::case_when(
      vote_2016_choice == 1 ~ "Clinton",
      vote_2016_choice == 2 ~ "Trump",
      TRUE ~ "Other")) %>%
  tidy_anova(pp_party ~ sex * vote_choice) %>%
  tidy_summary()

# t-tests
polcom %>%
  tidy_ttest(pp_ideology ~ follow_trump) %>%
  tidy_summary()

# Structural equation modeling (SEM)
polcom %>%
  dplyr::mutate(therm_2 = 10 - therm_2 / 10,
    therm_1 = therm_1 / 10) %>%
  tidy_sem(news =~ news_1 + news_2 + news_3 + news_4 + news_5 + news_6,
    ambiv_sexism =~ ambiv_sexism_1 + ambiv_sexism_2 + ambiv_sexism_3 +
      ambiv_sexism_4 + ambiv_sexism_5 + ambiv_sexism_6,
    partisan =~ a*therm_1 + a*therm_2,
    ambiv_sexism ~ age + hhinc + edu + news + partisan) %>%
  tidy_summary()

# Cronbach's alpha
cronbachs_alpha(polcom, ambiv_sexism_1:ambiv_sexism_6)

# Analysis: ppcor’s pcor.test() -------------------------------------------

# Load package
library(ppcor)

# Get data
y.data <- data.frame(
  hl = c(7,15,19,15,21,22,57,15,20,18),
  disp = c(0.000,0.964,0.000,0.000,0.921,0.000,0.000,1.006,0.000,1.011),
  deg = c(9,2,3,4,1,3,1,3,6,1),
  BC = c(1.78e-02,1.05e-06,1.37e-05,7.18e-03,0.00e+00,0.00e+00,0.00e+00,
    4.48e-03,2.10e-06,0.00e+00)
)

# Run analysis
pcor_correlation <- pcor.test(y.data$hl, y.data$disp, y.data[,c("deg","BC")])
pcor_correlation

# Inspect model -----------------------------------------------------------

inspect(results)
inspect(lm_)
inspect(results, glm_gamma)




cox

cox <- mutate(cox, MS = if_else(condition == "mortality salience", 1, 0))

lm_simple <- lm(call_parent ~ condition, data = cox)
summary(lm_simple)

library(psych)

model <- lm(PA2 ~ factor(Film), data = affect)
summary(model)





# Test updating -----------------------------------------------------------

# Create two empty tidystats lists
results_wrong <- list()
results_good <- list()

# Perform a wrond and incorrect test
t_test_wrong <- t.test(extra ~ group, data = sleep)
t_test_good <- t.test(extra ~ group, data = sleep, paired = TRUE)

# Add analysis to the first tidystats list
results_wrong <- add_stats(results_wrong, t_test_wrong, 
  identifier = "main_test")

# And to the second, making sure the identifier is the same between the lists
results_good <- add_stats(results_good, t_test_good, 
  identifier = "main_test")

# Save the results
write_stats(results_wrong, "docs/tests/results_wrong.json")
write_stats(results_good, "docs/tests/results_good.json")


temp <- jsonlite::read_json("docs/tests/test_correct.json")
temp2 <- jsonlite::fromJSON("docs/tests/test_correct.json")


jsonlite::fromJSON("docs/tests/test_correct.json") %>%
  map_df(as.data.frame)

# Covariance matrix

vcov(lm_multiple)

summary(lm_simple)
vcov(lm_simple, complete = F)

summary_simple <- summary(lm_simple)

cox %>%
  mutate(
    condition2 = if_else(condition == "mortality salience", 1, 0),
    intercept = mean(call_parent)) %>%
  select(call_parent, intercept, condition2) %>%
  cov()

summary_simple$cov


summary_simple$cov.unscaled * summary_simple$sigma^2
tidy_vcov <- vcov(lm_interaction) %>%
  as_tibble(rownames = "coefficient1") %>%
  gather("coefficient2", "value", -coefficient1) %>%
  mutate(
    statistic = if_else(coefficient1 == coefficient2, "variance", 
      "covariance"),
    coefficient2 = if_else(statistic == "variance", NA_character_, 
      coefficient2)
  ) %>%
  unite(term, coefficient1, coefficient2, sep = " * ") %>%
  mutate(term = str_remove(term, " \\* NA"))

tidy_vcov %>%
  separate(term, into = c("coefficient1", "coefficient2"), sep = " \\* ", 
    fill = "right") %>%
  select(-statistic) %>%
  mutate(coefficient2 = if_else(is.na(coefficient2), coefficient1, 
    coefficient2)) %>%
  spread(coefficient2, value)



lm_interaction <- lm(call_parent ~ condition, data = cox)
summary(lm_interaction)

data <- cox %>%
  mutate(condition2 = if_else(condition == "mortality salience", 1, 0)) %>%
  select(call_parent, condition2)
  

cov(data)

b <- 0.6959799 / 0.25125628
a <- mean(data$call_parent) - b * mean(data$condition2)

a
b

#
# 1. Generate some data.
#
n <- 10        # Data set size
p <- 2         # Number of regressors
set.seed(17)
z <- matrix(rnorm(n*(p+1)), nrow=n, dimnames=list(NULL, paste0("x", 1:(p+1))))
y <- z[, p+1]
x <- z[, -(p+1), drop=FALSE]; 
#
# 2. Find the OLS coefficients from the covariances only.
#
a <- cov(x)
b <- cov(x,y)
beta.hat <- solve(a, b)[, 1]  # Coefficients from the covariance matrix

lm(x3 ~ x1 + x2, data = data.frame(z))

#
# 2a. Find the intercept from the means and coefficients.
#
y.bar <- mean(y)
x.bar <- colMeans(x)
intercept <- y.bar - x.bar %*% beta.hat  
