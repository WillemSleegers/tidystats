
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(metafor)

# Create an empty list
results <- list()

# rma.uni() --------------------------------------------------------------------

# Get data
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

### fit a random-effects model using the log risk ratios and sampling variances as input
rma_uni <- rma(yi, vi, data=dat, method="REML")

### fit a mixed-effects model with two moderators (absolute latitude and publication year)
rma_uni_mods <- rma(yi ~ ablat + year, vi, data=dat)

### test all pairwise differences with Holm's method (using the 'multcomp' package if installed)
rma_uni_pairwise <- rma(yi, vi, mods = ~ factor(alloc) - 1, data=dat)

### demonstrating that Q_E + Q_M = Q_Total for fixed-effects models
rma_uni_qtotal <- rma(yi, vi, data=dat, method="FE")
### Q_E + Q_M
rma_uni_qs <- rma(yi, vi, mods = ~ ablat + year, data=dat, method="FE")

### an example of a location-scale model
dat <- dat.bangertdrowns2004

### fit as location-scale model
rma_uni_ls <- rma(yi, vi, scale = ~ 1, data=dat)

### add the total sample size (per 100) as a location and scale predictor
dat$ni100 <- dat$ni/100
rma_uni_ls_pred <- rma(yi, vi, mods = ~ ni100, scale = ~ ni100, data=dat)

### variables in the location and scale parts can differ
rma_uni_ls_diff <- rma(yi, vi, mods = ~ ni100 + meta, scale = ~ ni100 + imag, data=dat)

# Add stats
results = results %>%
  add_stats(rma_uni) %>%
  add_stats(rma_uni_mods) %>%
  add_stats(rma_uni_pairwise) %>%
  add_stats(rma_uni_qtotal) %>%
  add_stats(rma_uni_qs) %>%
  add_stats(rma_uni_ls) %>%
  add_stats(rma_uni_ls_pred) %>%
  add_stats(rma_uni_ls_diff)

# Inspect output
rma_uni
rma_uni_mods
rma_uni_pairwise
rma_uni_qtotal
rma_uni_qs
rma_uni_ls
rma_uni_ls_pred
rma_uni_ls_diff


# rma.mh() --------------------------------------------------------------------

# Run analyses

### meta-analysis of the (log) odds ratios using the Mantel-Haenszel method
rma_mh_test_or <- rma.mh(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

### meta-analysis of the (log) risk ratios using the Mantel-Haenszel method
rma_mh_test_rr <- rma.mh(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

# Add stats
results <- results %>%
  add_stats(rma_mh_test_or) %>%
  add_stats(rma_mh_test_rr)

# Inspect output
rma_mh_test_or
rma_mh_test_rr

# rma.peto() --------------------------------------------------------------------

# Run analyses

### meta-analysis of the (log) odds ratios using Peto's method
rma_peto_test <- rma.peto(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

# Add stats
results <- results %>%
  add_stats(rma_peto_test)

# Inspect output
rma_peto_test


# rma.glmm() --------------------------------------------------------------------

# Run analyses: random-effects models using rma.glmm() (require 'lme4' package)

### unconditional model with fixed study effects
rma_glmm_umfs = rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, model="UM.FS")

### unconditional model with random study effects
rma_glmm_umrs = rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, model="UM.RS")

### conditional model with approximate likelihood
rma_glmm_cmal = rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, model="CM.AL")

### conditional model with exact likelihood (takes too long)
#rma_glmm_cmel = rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, model="CM.EL")

# Add stats
results <- results %>%
  add_stats(rma_glmm_umfs) %>%
  add_stats(rma_glmm_umrs) %>%
  add_stats(rma_glmm_cmal) # %>% add_stats(rma_glmm_cmel)

# Inspect output
rma_glmm_umfs
rma_glmm_umrs
rma_glmm_cmal
#rma_glmm_cmel


# rma.mv() --------------------------------------------------------------------

# Get data
### calculate log odds ratios and corresponding sampling variances
dat <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

# Run analyses

### fit random-effects model using rma.mv()
rma_mv = rma.mv(yi, vi, random = ~ 1 | trial, data=dat)

### multilevel model with random effects
rma_mv_mm = rma.mv(yi, vi, random = ~ 1 | district/school, data=dat.konstantopoulos2011)

### change data into long format
dat.long <- to.long(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
### set levels of group variable ("exp" = experimental/vaccinated; "con" = control/non-vaccinated)
levels(dat.long$group) <- c("exp", "con")
### set "con" to reference level
dat.long$group <- relevel(dat.long$group, ref="con")
### calculate log odds and corresponding sampling variances
dat.long <- escalc(measure="PLO", xi=out1, mi=out2, data=dat.long)

### fit bivariate random-effects model using rma.mv()
rma_mv_biv <- rma.mv(yi, vi, mods = ~ group, random = ~ group | study, struct="UN", data=dat.long)


results <- results %>%
  add_stats(rma_mv)

# Add stats
results <- results %>%
  add_stats(rma_mv) %>%
  add_stats(rma_mv_mm) %>%
  add_stats(rma_mv_biv)


# Inspect output
rma_mv
rma_mv_mm
rma_mv_biv


# confint.rma() --------------------------------------------------------------------

# Get data
### calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

# Run analyses

### meta-analysis of the log risk ratios using a random-effects model
confint_rma_uni <- confint(res <- rma(yi, vi, data=dat, method="REML"))

### multilevel random-effects model
confint_rma_mv <- confint(rma.mv(yi, vi, random = ~ 1 | district/school, data=dat.konstantopoulos2011))

### multivariate parameterization of the model
res = rma.mv(yi, vi, random = ~ school | district, data=dat.konstantopoulos2011)
confint_rma_mv_para <- confint(res)
confint_rma_mv_single <- confint(res, tau2=1)
confint_rma_mv_ci80 <- confint(res, level = .8)

### rma.mh (same as peta)
confint_rma_mh <- confint(rma.mh(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg))

# Add stats
results <- results %>%
  add_stats(confint_rma_uni) %>%
  add_stats(confint_rma_mv) %>%
  add_stats(confint_rma_mv_para) %>%
  add_stats(confint_rma_mv_single) %>%
  add_stats(confint_rma_mv_ci80, args = .8) %>%
  add_stats(confint_rma_mh)

# Inspect output
confint_rma_uni
confint_rma_mv
confint_rma_mv_para
confint_rma_mv_single
confint_rma_mv_ci80
confint_rma_mh

# anova.rma() --------------------------------------------------------------------

### calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

### fit random-effects model
res1 <- rma(yi, vi, data=dat, method="ML")

### fit mixed-effects model with two moderators (absolute latitude and publication year)
res2 <- rma(yi, vi, mods = ~ ablat + year, data=dat, method="ML")

### Wald-type test of the two moderators
anova_rma_wald <- anova(res2)

### alternative way of specifying the same test
anova_rma_wald_est <- anova(res2, X=rbind(c(0,1,0), c(0,0,1)))

### corresponding likelihood ratio test
anova_rma_lrt <- anova(res1, res2)

### Wald-type test of a linear combination
anova_rma_wald_comb <- anova(res2, X=c(1,35,1970))

### an example of doing LRTs of variance components in more complex models
dat <- dat.konstantopoulos2011
res <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat)
### likelihood ratio test of the district-level variance component
res0 <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat, sigma2=c(0,NA))
anova_rma_lrt_complex = anova(res, res0)

results <- results %>%
  add_stats(anova_rma_wald_comb)

# Add stats
results <- results %>%
  add_stats(anova_rma_wald) %>%
  add_stats(anova_rma_wald_est) %>%
  add_stats(anova_rma_lrt) %>%
  add_stats(anova_rma_wald_comb) %>%
  add_stats(anova_rma_lrt_complex)

# Inspect output
anova_rma_wald
anova_rma_wald_est
anova_rma_lrt
anova_rma_wald_comb
anova_rma_lrt_complex


# permutest() --------------------------------------------------------------------

# Get data
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

# Run analyses
### random-effects model
res <- rma(yi, vi, data=dat)

### permutation test (approximate and exact)
set.seed(1234) # for reproducibility
permutest_single = permutest(res, iter = 5)

### mixed-effects model with two moderators (absolute latitude and publication year)
res <- rma(yi, vi, mods = ~ ablat + year, data=dat)

### permutation test (approximate only; exact not feasible)
set.seed(1234) # for reproducibility
permutest_mods <- permutest(res, iter=15)

### permutation test for rma.ls
dat <- dat.bangertdrowns2004
### add the total sample size (per 100) as a location and scale predictor
dat$ni100 <- dat$ni/100
### add the total sample size (per 100) as a location and scale predictor
rma_uni_ls_sample <- rma(yi, vi, mods = ~ ni100, scale = ~ ni100, data=dat)
set.seed(1234) # for reproducibility
permutest_ls <- permutest(rma_uni_ls_sample, iter=5)

# Add stats
results <- results %>%
  add_stats(permutest_single) %>%
  add_stats(permutest_mods) %>%
  add_stats(permutest_ls)

# Inspect output
permutest_single
permutest_mods
permutest_ls


# tes() --------------------------------------------------------------------

# Get data

### calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=x.a, n1i=n.a, ci=x.p, n2i=n.p, data=dat.dorn2007)

### conduct test of excess significance (using test="chi2" to speed things up)
tes_result = tes(yi, vi, data=dat, test="chi2")

# Add stats
results <- results %>%
  add_stats(tes_result)

# Inspect output
tes_result


# matreg() --------------------------------------------------------------------

### copy data into 'dat'
dat <- dat.craft2003
### construct dataset and var-cov matrix of the correlations
tmp <- rcalc(ri ~ var1 + var2 | study, ni=ni, data=dat)
V <- tmp$V
dat <- tmp$dat
### turn var1.var2 into a factor with the desired order of levels
dat$var1.var2 <- factor(dat$var1.var2,
   levels=c("acog.perf", "asom.perf", "conf.perf", "acog.asom", "acog.conf", "asom.conf"))
### multivariate random-effects model
res <- rma.mv(yi, V, mods = ~ var1.var2 - 1, random = ~ var1.var2 | study, struct="UN", data=dat)
### restructure estimated mean correlations into a 4x4 matrix
R <- vec2mat(coef(res))
rownames(R) <- colnames(R) <- c("perf", "acog", "asom", "conf")
### fit regression model with 'perf' as outcome and 'acog', 'asom', and 'conf' as predictors
matreg_base <- matreg(1, 2:4, R=R, V=vcov(res))

### a different example based on van Houwelingen et al. (2002)
### create dataset in long format
dat.long <- to.long(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.colditz1994)
dat.long <- escalc(measure="PLO", xi=out1, mi=out2, data=dat.long)
dat.long$tpos <- dat.long$tneg <- dat.long$cpos <- dat.long$cneg <- NULL
levels(dat.long$group) <- c("CON", "EXP")
### fit bivariate model
res <- rma.mv(yi, vi, mods = ~ group - 1, random = ~ group | trial, struct="UN",
              data=dat.long, method="ML")
### regression of log(odds)_EXP on log(odds)_CON
matreg_biv <- matreg(y=2, x=1, R=res$G, cov=TRUE, means=coef(res), n=res$g.levels.comb.k)

# Add stats
results <- results %>%
  add_stats(matreg_base) %>%
  add_stats(matreg_biv)

# Inspect output
matreg_base
matreg_biv


# ranktest() --------------------------------------------------------------------

### calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
### carry out the rank correlation test
rank_test <- ranktest(yi, vi, data=dat)

# Add stats
results <- results %>%
  add_stats(rank_test)

# Inspect output
rank_test

# regtest() --------------------------------------------------------------------

### copy data into 'dat' and examine data
dat <- dat.egger2001
### calculate log odds ratios and corresponding sampling variances (but remove ISIS-4 trial)
dat <- escalc(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat, subset=-16)
### classical Egger test
regtest_egge <- regtest(yi, vi, data=dat, model="lm")
### mixed-effects meta-regression version of the Egger test
regtest_mixed <- regtest(yi, vi, data=dat)
### predictor specified
regtest_pred <- regtest(yi, vi, data=dat, predictor="ni")

# Add stats
results <- results %>%
  add_stats(regtest_egge) %>%
  add_stats(regtest_mixed) %>%
  add_stats(regtest_pred)

# Inspect output
regtest_egge
regtest_mixed
regtest_pred


# trimfill() --------------------------------------------------------------------

### calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
### meta-analysis of the log risk ratios using an equal-effects model
res <- rma(yi, vi, data=dat, method="EE")
trimfill_result <- trimfill(res)

# Add stats
results <- results %>%
  add_stats(trimfill_result)

# Inspect output
trimfill_result

# selmodel() --------------------------------------------------------------------

### example from Citkowicz and Vevea (2017) for beta selection model
# copy data into 'dat' and examine data
dat <- dat.baskerville2012
# fit random-effects model
res <- rma(smd, se^2, data=dat, method="ML", digits=3)
# fit beta selection model
selmodel_beta <- selmodel(res, type="beta")

### example from Preston et al. (2004)
# calculate log odds ratios and corresponding sampling variances
dat <- escalc(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat.hahn2001, drop00=TRUE)
# fit equal-effects model
res <- rma(yi, vi, data=dat, method="EE")
# fit half-normal selection models
selmodel_halfnorm <- selmodel(res, type="halfnorm", alternative="less")

### meta-analysis on the effect of environmental tobacco smoke on lung cancer risk
# copy data into 'dat' and examine data
dat <- dat.hackshaw1998
# fit random-effects model
res <- rma(yi, vi, data=dat, method="ML")
# step function selection model
selmodel_stepfun <- selmodel(res, type="stepfun", alternative="greater", steps=c(.025,.10,.50,1))

# Add stats
results <- results %>%
  add_stats(selmodel_beta) %>%
  add_stats(selmodel_halfnorm) %>%
  add_stats(selmodel_stepfun)

# Inspect output
selmodel_beta
selmodel_halfnorm
selmodel_stepfun

# fsn() --------------------------------------------------------------------

### calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
### fail-safe N computations
fsn_rosenthal <- fsn(yi, vi, data=dat)
fsn_orwin <- fsn(yi, data=dat, type="Orwin", target=log(0.95)) # target corresponds to a 5% risk reduction
fsn_rosenberg <- fsn(yi, vi, data=dat, type="Rosenberg")

# Add stats
results <- results %>%
  add_stats(fsn_rosenthal) %>%
  add_stats(fsn_orwin) %>%
  add_stats(fsn_rosenberg)

# Inspect output
fsn_rosenthal
fsn_orwin
fsn_rosenberg

# hc() --------------------------------------------------------------------

### calculate log odds ratios and corresponding sampling variances
dat <- escalc(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat.lee2004)
### meta-analysis based on log odds ratios
res <- rma(yi, vi, data=dat)
### use method by Henmi and Copas (2010) as a sensitivity analysis
hc_standard <- hc(res)
### back-transform results to odds ratio scale
hc_tranformed <- hc(res, transf=exp)

# Add stats
results <- results %>%
  add_stats(hc_standard) %>%
  add_stats(hc_tranformed)

# Inspect output
hc_standard
hc_tranformed

# robust() --------------------------------------------------------------------
### copy data from Bangert-Drowns et al. (2004) into 'dat'
dat <- dat.bangertdrowns2004
### fit random-effects model
res <- rma(yi, vi, data=dat)
### obtain results based on the sandwich method
robust_standard <- robust(res, cluster=id)

### copy data from Konstantopoulos (2011) into 'dat'
dat <- dat.konstantopoulos2011
### fit multilevel random-effects model
res <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat)
### obtain results based on the sandwich method
robust_multi_rem <- robust(res, cluster=district)

### copy data from Berkey et al. (1998) into 'dat'
dat <- dat.berkey1998
V <- vcalc(vi=1, cluster=author, rvars=c(v1i, v2i), data=dat)
### fit multivariate model
res <- rma.mv(yi, V, mods = ~ outcome - 1, random = ~ outcome | trial, struct="UN", data=dat)
### obtain results based on sandwich method
robust_mv <- robust(res, cluster=trial)

# Add stats
results <- results %>%
  add_stats(robust_standard) %>%
  add_stats(robust_multi_rem) %>%
  add_stats(robust_mv)

# Inspect output
robust_standard
robust_multi_rem
robust_mv

# cumul() --------------------------------------------------------------------

### calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
### fit random-effects model
res <- rma(yi, vi, data=dat)
### cumulative meta-analysis (in the order of publication year)
cumul_uni = cumul(res, transf=exp, order=year)

### meta-analysis of the (log) risk ratios using the Mantel-Haenszel method
res <- rma.mh(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
### cumulative meta-analysis
cumul_mh <- cumul(res, order=year)

# Add stats
results <- results %>%
  add_stats(cumul_uni) %>%
  add_stats(cumul_mh)

# Inspect output
cumul_uni
cumul_mh

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/metafor.json")
