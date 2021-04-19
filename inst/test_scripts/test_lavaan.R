
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(tidystats)
library(lavaan)

# Create an empty list
results <- list()

# lavaan ------------------------------------------------------------------

# The Holzinger and Swineford (1939) example
HS_model <- 'visual  =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9'

fit <- lavaan(HS_model, data = HolzingerSwineford1939, auto.var = TRUE, 
  auto.fix.first = TRUE, auto.cov.lv.x = TRUE)
summary(fit, fit.measures = TRUE)

# Tidy stats
x <- tidy_stats(fit, args = list(fit.measures = TRUE))

# Add stats
results <- add_stats(results, fit, args = list(fit.measures = TRUE))

# cfa ---------------------------------------------------------------------

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data=HolzingerSwineford1939)
fit
summary(fit, fit.measures = TRUE)

summary(fit, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)
fitted(fit)
coef(fit)
resid(fit, type="normalized")