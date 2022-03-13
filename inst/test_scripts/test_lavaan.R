

# Todo --------------------------------------------------------------------

# - Add support for groups
# - Add support for intercepts
# - Add support for Defined Parameters
# - Add support for fitted values
# - Add support for residuals

# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(tidystats)
library(lavaan)

# Create an empty list
results <- list()

# cfa ---------------------------------------------------------------------

HS.model <- "visual  =~ x1 + x2 + x3 
             textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9"

fit <- cfa(HS.model, data = HolzingerSwineford1939)
summary(fit)
summary(fit, fit.measures = TRUE)

temp <- tidy_stats(fit)
temp <- tidy_stats(fit, args = list(fit.measures = FALSE))

HS.model <- "# three-factor model
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
               x9 ~ 1"

fit_intercepts <- cfa(HS.model, data = HolzingerSwineford1939, 
  meanstructure = TRUE)
summary(fit_intercepts)

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit_groups <- cfa(HS.model, data = HolzingerSwineford1939, group = "school")
summary(fit_groups)


# SEM ---------------------------------------------------------------------

model <- "# measurement model
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
            y6 ~~ y8"

fit <- sem(model, data = PoliticalDemocracy)
summary(fit, standardized = TRUE)

temp <- tidy_stats(fit, args = list(standardized = TRUE))

set.seed(1234)
X <- rnorm(100)
M <- 0.5*X + rnorm(100)
Y <- 0.7*M + rnorm(100)
Data <- data.frame(X = X, Y = Y, M = M)
model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit_defined_parameters <- sem(model, data = Data)
summary(fit_defined_parameters)

# lavaan ------------------------------------------------------------------

# The Holzinger and Swineford (1939) example
HS_model <- 'visual  =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9'

fit <- lavaan(HS_model, data = HolzingerSwineford1939, auto.var = TRUE, 
  auto.fix.first = TRUE, auto.cov.lv.x = TRUE)
summary(fit)
summary(fit, fit.measures = TRUE)
summary(fit, standardized = TRUE)
summary(fit, ci = TRUE)
summary(fit, fit.measures = TRUE, standardized = TRUE, ci = TRUE)

# Tidy stats
x <- tidy_stats.lavaan(fit, args = list(fit.measures = TRUE))
x <- tidy_stats.lavaan(fit, args = list(standardized = TRUE))
x <- tidy_stats.lavaan(fit, args = list(ci = TRUE))

# Add stats
results <- add_stats(results, fit, args = list(fit.measures = TRUE))
