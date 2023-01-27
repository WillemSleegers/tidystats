
# Analysis: afex ----------------------------------------------------------

# Analysis: psych ---------------------------------------------------------

# Load package
library(psych)


# Analysis: psych's ICC ---------------------------------------------------

# Load data
sf <- matrix(
  ncol = 4, byrow = TRUE,
  c(
    9, 2, 5, 8,
    6, 1, 3, 2,
    8, 4, 6, 8,
    7, 1, 2, 6,
    10, 5, 6, 9,
    6, 2, 4, 7
  )
)
colnames(sf) <- paste("J", 1:4, sep = "")
rownames(sf) <- paste("S", 1:6, sep = "")

# Perform analysis
psych_ICC <- ICC(sf, lmer = FALSE)

# Tidy stats
tidy_stats(psych_ICC)

# Add stats
results <- add_stats(results, psych_ICC)


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
dat <- escalc(
  measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg,
  data = dat.bcg
)

# Run univariate meta-analyses
rma_uni <- rma(yi, vi, data = dat, method = "REML", level = 90)
rma_uni_mods <- rma(yi, vi,
  mods = cbind(ablat, year), data = dat,
  method = "REML"
)

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
dat.long <- to.long(
  measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg,
  data = dat.bcg
)

# Set levels of group variable
levels(dat.long$group) <- c("exp", "con")

# Set "con" to reference level
dat.long$group <- relevel(dat.long$group, ref = "con")

# Calculate log odds and corresponding sampling variances
dat.long <- escalc(measure = "PLO", xi = out1, mi = out2, data = dat.long)
dat.long$effect <- 1:nrow(dat.long)

# Bivariate random-effects model using rma.mv()
rma_mv <- rma.mv(yi, vi,
  random = ~ group | study / effect, struct = "UN",
  data = dat.long
)
rma_mv_mods <- rma.mv(yi, vi,
  mods = ~group, random = ~ group | study,
  struct = "UN", data = dat.long
)

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

res <- rma(
  ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg, measure = "RR",
  slab = paste(author, year, sep = ", "), method = "REML"
)
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
rcorr_correlations <- rcorr(cbind(x, y, z, v))
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
output <- select(output, c(
  "var",
  names(sort(colSums(is.na(select(output, -var))),
    decreasing = T
  ))
))

# Analysis: anova()
anova(model3_1)

anova(model3_1, model3_2)

anova(model5_1)

anova(model5_1, model5_2)


# MANOVA

data <- iris

model7_1 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
  data = iris
), test = "Roy")
model7_2 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
  data = iris
), test = "")
model7_3 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
  data = iris
), test = "Roy")

model7_4 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species *
  Petal.Width, data = iris), test = "Roy")

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
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1,
    type = "quasipoisson",
    robust = TRUE
  ) %>%
  tidy_summary()

# ANOVA
polcom %>%
  dplyr::mutate(
    sex = ifelse(sex == 1, "Male", "Female"),
    vote_choice = dplyr::case_when(
      vote_2016_choice == 1 ~ "Clinton",
      vote_2016_choice == 2 ~ "Trump",
      TRUE ~ "Other"
    )
  ) %>%
  tidy_anova(pp_party ~ sex * vote_choice) %>%
  tidy_summary()

# t-tests
polcom %>%
  tidy_ttest(pp_ideology ~ follow_trump) %>%
  tidy_summary()

# Structural equation modeling (SEM)
polcom %>%
  dplyr::mutate(
    therm_2 = 10 - therm_2 / 10,
    therm_1 = therm_1 / 10
  ) %>%
  tidy_sem(
    news = ~ news_1 + news_2 + news_3 + news_4 + news_5 + news_6,
    ambiv_sexism = ~ ambiv_sexism_1 + ambiv_sexism_2 + ambiv_sexism_3 +
      ambiv_sexism_4 + ambiv_sexism_5 + ambiv_sexism_6,
    partisan = ~ a * therm_1 + a * therm_2,
    ambiv_sexism ~ age + hhinc + edu + news + partisan
  ) %>%
  tidy_summary()

# Cronbach's alpha
cronbachs_alpha(polcom, ambiv_sexism_1:ambiv_sexism_6)

# Analysis: ppcor’s pcor.test() -------------------------------------------

# Load package
library(ppcor)

# Get data
y.data <- data.frame(
  hl = c(7, 15, 19, 15, 21, 22, 57, 15, 20, 18),
  disp = c(0.000, 0.964, 0.000, 0.000, 0.921, 0.000, 0.000, 1.006, 0.000, 1.011),
  deg = c(9, 2, 3, 4, 1, 3, 1, 3, 6, 1),
  BC = c(
    1.78e-02, 1.05e-06, 1.37e-05, 7.18e-03, 0.00e+00, 0.00e+00, 0.00e+00,
    4.48e-03, 2.10e-06, 0.00e+00
  )
)

# Run analysis
pcor_correlation <- pcor.test(y.data$hl, y.data$disp, y.data[, c("deg", "BC")])
pcor_correlation
