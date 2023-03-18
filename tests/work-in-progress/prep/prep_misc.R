# MANOVA

data <- iris

model7_1 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
  data = iris
), test = "Roy")

model7_1

model7_2 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
  data = iris
), test = "Pillai")
model7_2

model7_3 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
  data = iris
), test = "Wilks")
model7_3

model7_4 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species *
  Petal.Width, data = iris), test = "Hotelling-Lawley")
model7_4

## Set orthogonal contrasts.
op <- options(contrasts = c("contr.helmert", "contr.poly"))
## Fake a 2nd response variable
npk2 <- within(npk, foo <- rnorm(24))
(npk2.aov <- manova(cbind(yield, foo) ~ block + N * P * K, npk2))
summary(npk2.aov)
(npk2.aovE <- manova(cbind(yield, foo) ~ N * P * K + Error(block), npk2))
summary(npk2.aovE)

# Analysis: ppcor’s pcor.test() -------------------------------------------

# Load package
library(ppcor)

# Get data
y.data <- data.frame(
  hl = c(7, 15, 19, 15, 21, 22, 57, 15, 20, 18),
  disp = c(0, 0.964, 0, 0, 0.921, 0, 0, 1.006, 0, 1.011),
  deg = c(9, 2, 3, 4, 1, 3, 1, 3, 6, 1),
  BC = c(
    1.78e-02, 1.05e-06, 1.37e-05, 7.18e-03, 0.00e+00, 0.00e+00, 0.00e+00,
    4.48e-03, 2.10e-06, 0.00e+00
  )
)

# Run analysis
pcor_correlation <- pcor.test(y.data$hl, y.data$disp, y.data[, c("deg", "BC")])
pcor_correlation
