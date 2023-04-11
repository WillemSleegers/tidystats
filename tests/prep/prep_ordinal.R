# Setup -------------------------------------------------------------------

library(ordinal)

statistics <- list()

# clm() -------------------------------------------------------------------

data(wine)

fm1 <- clm(rating ~ temp * contact, data = wine)

statistics <- statistics |>
  add_stats(fm1)

summary(fm1)

# anova() -----------------------------------------------------------------

fm1 <- clm(rating ~ temp * contact, data = wine)
fm2 <- update(fm1, ~ . - temp:contact)

anova <- anova(fm1, fm2)

statistics <- statistics |>
  add_stats(anova)

anova

# update() ----------------------------------------------------------------

fm1_prt <- update(fm1, link = "probit")
fm1_ll <- update(fm1, link = "loglog")
fm1_cll <- update(fm1, link = "cloglog")
fm1_cct <- update(fm1, link = "cauchit")
fm1_symmetric <- update(fm1, threshold = "symmetric")
fm1_equidistant <- update(fm1, threshold = "equidistant")

statistics <- statistics |>
  add_stats(fm1_prt) |>
  add_stats(fm1_ll) |>
  add_stats(fm1_cll) |>
  add_stats(fm1_cct) |>
  add_stats(fm1_symmetric) |>
  add_stats(fm1_equidistant)

summary(fm1_prt)
summary(fm1_cll)
summary(fm1_ll)
summary(fm1_cct)
summary(fm1_symmetric)
summary(fm1_equidistant)

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(statistics)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/data/ordinal.json")

# Cleanup -----------------------------------------------------------------

rm(
  fm1, fm2, anova, fm1_prt, fm1_cll, fm1_ll, fm1_cct, fm1_symmetric,
  fm1_equidistant, df, statistics, wine
)
