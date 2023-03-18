# Setup -------------------------------------------------------------------

library(nlme)

statistics <- list()

# lme() -------------------------------------------------------------------

lme_fm1 <- lme(
  distance ~ age,
  random = ~ 1 + age | Subject / Sex,
  data = Orthodont
)
lme_fm2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~1)

statistics <- statistics |>
  add_stats(lme_fm1) |>
  add_stats(lme_fm2)

summary(lme_fm1)
summary(lme_fm2)


lme(
  distance ~ age,
  random = ~ 1 + age | Subject / Sex,
  data = Orthodont
)




# nlme() ------------------------------------------------------------------

# Run models
nlme_fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
  data = Loblolly,
  fixed = Asym + R0 + lrc ~ 1, random = Asym ~ 1,
  start = c(Asym = 103, R0 = -8.5, lrc = -3.3)
)
nlme_fm2 <- update(nlme_fm1, random = pdDiag(Asym + lrc ~ 1))

summary(nlme_fm1)
summary(nlme_fm2)

# Tidy stats
temp <- tidy_stats(nlme_fm1)
temp <- tidy_stats(nlme_fm2)

# Add stats
results <- results |>
  add_stats(nlme_fm1) |>
  add_stats(nlme_fm2)

# gls() -------------------------------------------------------------------

# Run models
gls_fm1 <- gls(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time), Ovary,
  correlation = corAR1(form = ~ 1 | Mare)
)
gls_fm2 <- update(gls_fm1, weights = varPower())

summary(gls_fm1)
summary(gls_fm2)

# Tidy stats
temp <- tidy_stats(gls_fm1)
temp <- tidy_stats(gls_fm2)

# Add stats
results <- results |>
  add_stats(gls_fm1) |>
  add_stats(gls_fm2)

# anova() -----------------------------------------------------------------

# Run models
fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
anova_fm1 <- anova(fm1)
anova_fm1

fm2 <- update(fm1, random = pdDiag(~age))
anova_fm1_fm2 <- anova(fm1, fm2)
anova_fm1_fm2

fm1Orth.gls <- gls(distance ~ Sex * I(age - 11), Orthodont,
  correlation = corSymm(form = ~ 1 | Subject),
  weights = varIdent(form = ~ 1 | age)
)
fm2Orth.gls <- update(fm1Orth.gls, corr = corCompSymm(form = ~ 1 | Subject))
anova_fm10_fm20 <- anova(fm1Orth.gls, fm2Orth.gls)
anova_fm10_fm20

fm3Orth.gls <- update(fm2Orth.gls, weights = NULL)
anova_fm20_fm30 <- anova(fm2Orth.gls, fm3Orth.gls)
anova_fm20_fm30

fm4Orth.gls <- update(fm3Orth.gls, weights = varIdent(form = ~ 1 | Sex))
anova_fm30_fm40 <- anova(fm3Orth.gls, fm4Orth.gls)
anova_fm30_fm40

fm3Orth.lme <- lme(distance ~ Sex * I(age - 11),
  data = Orthodont,
  random = ~ I(age - 11) | Subject,
  weights = varIdent(form = ~ 1 | Sex)
)

anova_fm30_fm40_no_test <- anova(fm3Orth.lme, fm4Orth.gls, test = FALSE)
anova_fm30_fm40_no_test

op <- options(contrasts = c("contr.treatment", "contr.poly"))

fm1BW.lme <- lme(weight ~ Time * Diet, BodyWeight, random = ~Time)
fm2BW.lme <- update(fm1BW.lme, weights = varPower())

anova_fm2BW <- anova(fm2BW.lme, L = c("Time:Diet2" = 1, "Time:Diet3" = -1))
anova_fm2BW

fm1Theo.lis <- nlsList(conc ~ SSfol(Dose, Time, lKe, lKa, lCl), data = Theoph)
fm1Theo.nlme <- nlme(fm1Theo.lis)
fm2Theo.nlme <- update(fm1Theo.nlme, random = pdDiag(lKe + lKa + lCl ~ 1))
fm3Theo.nlme <- update(fm2Theo.nlme, random = pdDiag(lKa + lCl ~ 1))

# Comparing the 3 nlme models
anova_fm1_fm3_fm2 <- anova(fm1Theo.nlme, fm3Theo.nlme, fm2Theo.nlme)
anova_fm1_fm3_fm2

options(op) # (set back to previous state)

# Tidy stats
temp <- tidy_stats(anova_fm1)
temp <- tidy_stats(anova_fm1_fm2)
temp <- tidy_stats(anova_fm10_fm20)
temp <- tidy_stats(anova_fm20_fm30)
temp <- tidy_stats(anova_fm30_fm40)
temp <- tidy_stats(anova_fm30_fm40_no_test)
temp <- tidy_stats(anova_fm2BW)
temp <- tidy_stats(anova_fm1_fm3_fm2)

# Add stats
results <- results %>%
  add_stats(anova_fm1) %>%
  add_stats(anova_fm1_fm2) %>%
  add_stats(anova_fm10_fm20) %>%
  add_stats(anova_fm20_fm30) %>%
  add_stats(anova_fm30_fm40) %>%
  add_stats(anova_fm30_fm40_no_test) %>%
  add_stats(anova_fm2BW) %>%
  add_stats(anova_fm1_fm3_fm2)

# Write stats -------------------------------------------------------------

write_stats(results, "inst/test_data/nlme.json")
