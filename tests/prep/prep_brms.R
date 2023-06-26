# Setup -------------------------------------------------------------------

library(brms)

statistics <- list()

# brm() -------------------------------------------------------------------

poisson_regression <- brm(
  count ~ zAge + zBase * Trt + (1 | patient),
  data = epilepsy,
  family = poisson(),
  prior = c(
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 2), class = sd)
  ),
  seed = 1,
  file = "./tests/models/brms-poisson-regression"
)

ordinal_regression <- brm(
  rating ~ period + carry + cs(treat),
  data = inhaler,
  family = sratio("logit"),
  prior = set_prior("normal(0,5)"),
  seed = 1,
  file = "./tests/models/brms-ordinal-regression"
)

survival_regression <- brm(
  time | cens(censored) ~ age * sex + disease + (1 | patient),
  data = kidney,
  family = lognormal(),
  seed = 1,
  file = "./tests/models/brms-survival-regression"
)

set.seed(1)
ntrials <- sample(1:10, 100, TRUE)
success <- rbinom(100, size = ntrials, prob = 0.4)
x <- rnorm(100)
data4 <- data.frame(ntrials, success, x)
probit_regression <- brm(
  success | trials(ntrials) ~ x,
  data = data4,
  family = binomial("probit"),
  file = "./tests/models/brms-probit-regression"
)

nonlinear_gaussian <- brm(
  bf(
    cum ~ ult * (1 - exp(-(dev / theta)^omega)),
    ult ~ 1 + (1 | AY),
    omega ~ 1,
    theta ~ 1,
    nl = TRUE
  ),
  data = loss, family = gaussian(),
  prior = c(
    prior(normal(5000, 1000), nlpar = "ult"),
    prior(normal(1, 2), nlpar = "omega"),
    prior(normal(45, 10), nlpar = "theta")
  ),
  control = list(adapt_delta = 0.9),
  seed = 1,
  file = "./tests/models/brms-nonlinear-gaussian"
)

set.seed(1)
heterogeneous_variances <- brm(
  bf(y ~ x, sigma ~ 0 + x),
  data = data.frame(
    y = c(rnorm(50), rnorm(50, 1, 2)),
    x = factor(rep(c("a", "b"), each = 50))
  ),
  file = "./tests/models/brms-heterogeneous-variances"
)
heterogeneous_variances

set.seed(1)
quantile_regression <- brm(
  bf(y ~ x, quantile = 0.25),
  data = data.frame(
    y = c(rnorm(50), rnorm(50, 1, 2)),
    x = factor(rep(c("a", "b"), each = 50))
  ),
  family = asym_laplace(),
  seed = 1,
  file = "./tests/models/brms-quantile-regression"
)

statistics <- statistics |>
  add_stats(poisson_regression) |>
  add_stats(ordinal_regression) |>
  add_stats(survival_regression) |>
  add_stats(nonlinear_gaussian) |>
  add_stats(heterogeneous_variances) |>
  add_stats(quantile_regression)

summary(poisson_regression)
summary(ordinal_regression)
summary(survival_regression)
summary(nonlinear_gaussian)
summary(heterogeneous_variances)
summary(quantile_regression)

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/brms.json")

# Cleanup -----------------------------------------------------------------

rm(
  poisson_regression, ordinal_regression, survival_regression,
  nonlinear_gaussian, heterogeneous_variances, quantile_regression, fit1,
  fit2, loo1, loo2, loo_compare, ntrials, success, x, data4, data_het, df,
  statistics
)
