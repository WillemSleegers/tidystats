# Setup -------------------------------------------------------------------

library(quantreg)

statistics <- list()

# rq() --------------------------------------------------------------------

data(stackloss)
data(engel)

rq_median <- rq(
  stack.loss ~ stack.x,
  tau = .5,
  stackloss
)

rq_sequence <- rq(
  foodexp ~ income,
  tau = c(.05, .1, .25, .75, .9, .95),
  engel
)

statistics <- statistics |>
  add_stats(rq_median) |>
  add_stats(rq_sequence)

summary(rq_median)
summary(rq_median, se = "boot")
summary(rq_sequence)

# crq() -------------------------------------------------------------------

# An artificial Powell example
set.seed(2345)
x <- sqrt(rnorm(100)^2)
y <- -0.5 + x + (.25 + .25 * x) * rnorm(100)
s <- (y > 0)
yLatent <- y
y <- pmax(0, y)
yc <- rep(0, 100)
tau <- 0.2
f <- crq(Curv(y, yc) ~ x, tau = tau, method = "Pow")
summary(f)

# crq example with left censoring
set.seed(1968)
n <- 200
x <- rnorm(n)
y <- 5 + x + rnorm(n)
c <- 4 + x + rnorm(n)
d <- (y > c)
f <- crq(survival::Surv(pmax(y, c), d, type = "left") ~ x, method = "Portnoy")

summary(f)

# anova() -----------------------------------------------------------------

data(barro)

fit1 <- rq(
  y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2,
  data = barro
)
fit2 <- rq(
  y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2,
  data = barro,
  tau = .75
)
fit3 <- rq(
  y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2,
  data = barro,
  tau = .25
)

anova_joint <- anova(fit1, fit2, fit3)
anova_distinct <- anova(fit1, fit2, fit3, joint = FALSE)

statistics <- statistics |>
  add_stats(anova_joint) |>
  add_stats(anova_distinct)

anova_joint
anova_distinct

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/quantreg.json")

# Cleanup -----------------------------------------------------------------

rm(
  rq_median, rq_sequence, stackloss, engel, fit1, fit2, fit3, anova_joint,
  anova_distinct, barro, df, statistics
)
