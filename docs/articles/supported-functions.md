# Supported functions

This page contains a list of all functions supported by tidystats.

Note that if a function is not yet supported, you can request support
for it by creating an
[issue](https://github.com/WillemSleegers/tidystats/issues) on GitHub or
by using the
[`custom_stats()`](https://willemsleegers.github.io/tidystats/reference/custom_stats.md)
function. See the
[`vignette("custom-statistics")`](https://willemsleegers.github.io/tidystats/articles/custom-statistics.md)
for more information.

## Packages and supported functions

### afex

- aov_ez()
- aov_car()
- aov_4()
- mixed()

### BayesFactor

- generalTestBF()
- lmBF()
- regressionBF()
- ttestBF()
- anovaBF()
- correlationBF()
- contingencyTableBF()
- proportionBF()
- meta.ttestBF()

### effectsize

- cohens_d()
- hedges_g()
- glass_delta()

### effsize

- cohen.d()
- VD.A()
- cliff.delta()

### emmeans

- emmeans()
- contrast()
- test()
- mvcontrast()
- eff_size()
- emtrends()
- joint_tests()
- ref_grid()

### Hmisc

- rcorr()

### irr

- icc()

### lme4

- lmer()
- anova()

### lmerTest

- lmer()
- anova()

### tidystats

- count_data()
- describe_data()

### stats

- anova()
- ansari.test()
- aov()
- bartlett.test()
- binom.test()
- Box.test()
- chisq.test()
- confint()
- cor.test()
- fisher.test()
- fligner.test()
- friedman.test()
- glm()
- kruskal.test()
- ks.test()
- lm()
- mantelhaen.test()
- mauchly.test()
- mcnemar.test()
- mood.test()
- oneway.test()
- pairwise.t.test()
- pairwise.prop.test()
- pairwise.wilcox.test()
- poisson.test()
- PP.test()
- prop.test()
- prop.trend.test()
- quade.test()
- shapiro.test()
- t.test()
- var.test()
- wilcox.test()
