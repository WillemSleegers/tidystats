

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
