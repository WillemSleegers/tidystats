# Todo --------------------------------------------------------------------

# - sem(): added defined parameter (mediation), multilevel, ESEM
# - Added growth() for latent growth curve models
# - lavTestLRT() for measurement invariance returns an object with fit
#   statistics
# - fitted values and residuals need the fit instead of summary object,
# but maybe not relevant for tidystats

# fitted covariance matrix (maybe not needed for tidystats)
# fitted.values(fit_cfa)
# unstandardized residual (maybe not needed for tidystats)
# resid(fit_cfa)

# Setup -------------------------------------------------------------------

library(lavaan)

statistics <- list()

# cfa() -------------------------------------------------------------------

HS_model <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
"
cfa <- cfa(HS_model, data = HolzingerSwineford1939)
cfa_groups <- cfa(HS_model, data = HolzingerSwineford1939, group = "school")

HS_model_intercepts <- "
    # three-factor model
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
    # intercepts
    x1 ~ 1
    x2 ~ 1
    x3 ~ 1
    x4 ~ 1
    x5 ~ 1
    x6 ~ 1
    x7 ~ 1
    x8 ~ 1
    x9 ~ 1
"
cfa_intercepts <- cfa(
  HS_model_intercepts,
  data = HolzingerSwineford1939,
  meanstructure = TRUE
)

efa_model <- '
    efa("efa")*f1 +
    efa("efa")*f2 +
    efa("efa")*f3 =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
'
cfa_efa <- cfa(efa_model, data = HolzingerSwineford1939)

statistics <- statistics |>
  add_stats(cfa) |>
  add_stats(cfa_groups) |>
  add_stats(cfa_intercepts) |>
  add_stats(cfa_efa)

summary(cfa, standardized = TRUE, fit.measures = TRUE)
summary(cfa_groups, standardized = TRUE, fit.measures = TRUE)
summary(cfa_intercepts, standardized = TRUE, fit.measures = TRUE)
summary(cfa_efa, standardized = TRUE, fit.measures = TRUE)

# sem() -------------------------------------------------------------------

sem_model <- "
    # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
    # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
"

sem <- sem(sem_model, data = PoliticalDemocracy)

set.seed(1234)
X <- rnorm(100)
M <- 0.5 * X + rnorm(100)
Y <- 0.7 * M + rnorm(100)
data <- data.frame(X = X, Y = Y, M = M)

sem_mediation_model <- "
    # direct effect
    Y ~ c*X
    # mediator
    M ~ a*X
    Y ~ b*M
    # indirect effect (a*b)
    ab := a*b
    # total effect
    total := c + (a*b)
"

mediation <- sem(sem_mediation_model, data = data)

sem_multilevel_model <- "
    level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2 + x3
    level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1 + w2
"

sem_multilevel <- sem(
  sem_multilevel_model,
  data = Demo.twolevel, cluster = "cluster"
)

ex5_25 <- read.table("http://statmodel.com/usersguide/chap5/ex5.25.dat")
names(ex5_25) <- paste0("y", 1:12)

model.esem <- '
    # efa block
    efa("efa1")*f1 +
    efa("efa1")*f2 =~ y1 + y2 + y3 + y4 + y5 + y6

    # cfa block
    f3 =~ y7 + y8 + y9
    f4 =~ y10 + y11 + y12

    # regressions
    f3 ~ f1 + f2
    f4 ~ f3
'
fit_esem <- sem(
  model = model.esem,
  data = ex5_25,
  rotation = "geomin",
  information = "observed",
  rotation.args = list(
    rstarts = 30,
    row.weights = "none",
    algorithm = "gpa",
    std.ov = TRUE,
    geomin.epsilon = 0.0001
  )
)
summary_esem <- summary(
  fit_esem,
  standardized = TRUE,
  fit.measures = TRUE
)

statistics <- statistics |>
  add_stats(summary_sem) |>
  add_stats(summary_definied_parameters) |>
  add_stats(summary_multilevel) |>
  add_stats(summary_esem)

summary(sem, standardized = TRUE, fit.measures = TRUE)
summary(mediation)
summary(multilevel, standardized = TRUE, fit.measures = TRUE)

# lavaan() ----------------------------------------------------------------

fit_lavaan <- lavaan(
  HS.model,
  data = HolzingerSwineford1939, auto.var = TRUE,
  auto.fix.first = TRUE, auto.cov.lv.x = TRUE
)

summary_lavaan <- summary(
  fit_lavaan,
  standardized = TRUE,
  fit.measures = TRUE
)

statistics <- statistics |>
  add_stats(summary_lavaan)


# lavTestLRT() ------------------------------------------------------------

HS_model <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
"

fit1 <- cfa(
  HS_model,
  data = HolzingerSwineford1939,
  group = "school"
)

fit2 <- cfa(
  HS_model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = "loadings"
)
fit3 <- cfa(
  HS_model,
  data = HolzingerSwineford1939,
  group = "school",
  group.equal = c("intercepts", "loadings")
)

lrt <- lavTestLRT(fit1, fit2, fit3)
lrt

# growth() ----------------------------------------------------------------

model_growth <- "
  i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
  s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
"

growth <- growth(model_growth, data = Demo.growth)

statistics <- add_stats(statistics, growth)

summary(growth, fit.measures = TRUE, standardized = TRUE)

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/lavaan.json")

# Cleanup -----------------------------------------------------------------

rm(
  statistics, summary_cfa, summary_intercepts, summary_efa,
  summary_invariance, summary_sem, summary_definied_parameters,
  summary_multilevel, summary_esem, summary_growth,
  summary_lavaan, fit_cfa, fit_intercepts, fit_groups, fit_efa,
  fit1, fit2, fit3, fit_sem, fit_defined_parameters, ex5_25,
  fit_esem, fit_multilevel, fit_growth, fith_lavaan, HS.model,
  HS.model.intercepts, efa.model, model, model.mediation,
  model.multilevel, model.esem, model.growth, df
)
