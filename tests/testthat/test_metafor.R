
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(metafor)

# Load test data
path <- system.file("tests/testthat/data/metafor.json", package = "tidystats")
test_results <- read_stats(path)

# Set options
tolerance <- 0.001

# Function to compare models
# model: the model to be passed to tidy_stats
# tidy_model_test: test results to compare to
models_equal = function(model, tidy_model_test) {
  tidy_model <- tidy_stats(model)
  tidy_model$package <- NULL
  tidy_model_test$package <- NULL
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
}

# Test: rma.uni ----------------------------------------------------------------

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

test_that("Regular random-effects model works",
  {
    models_equal(
      rma(yi, vi, data=dat, method="REML"),
      test_results$rma_uni)
  })

test_that("Mixed-effects model with moderators works",
  {
    models_equal(
      rma(yi ~ ablat + year, vi, data=dat),
      test_results$rma_uni_mods)
  })


test_that("RMA for pairwise differences (with Holm's method) works",
  {
    models_equal(
      rma(yi, vi, mods = ~ factor(alloc) - 1, data=dat),
      test_results$rma_uni_pairwise)
  })

test_that("RMA with Q_Total for fixed-effects works",
  {
    models_equal(
      rma(yi, vi, data=dat, method="FE"),
      test_results$rma_uni_qtotal)
  })

test_that("RMA with Q_E + Q_M for fixed-effects works",
  {
    models_equal(
      rma(yi, vi, mods = ~ ablat + year, data=dat, method="FE"),
      test_results$rma_uni_qs)
  })

dat <- dat.bangertdrowns2004

test_that("RMA for location-scale model works",
  {
    models_equal(
      rma(yi, vi, scale = ~ 1, data=dat),
      test_results$rma_uni_ls)
  })

test_that("RMA for location-scale model with scale predictor works",
  {
    dat$ni100 <- dat$ni/100
    models_equal(
      rma(yi, vi, mods = ~ ni100, scale = ~ ni100, data=dat),
      test_results$rma_uni_ls_pred)
  })

test_that("RMA for location-scale model with differing location and scale parts works",
  {
    dat$ni100 <- dat$ni/100
    models_equal(
      suppressWarnings(rma(yi, vi, mods = ~ ni100 + meta, scale = ~ ni100 + imag, data=dat)),
      test_results$rma_uni_ls_diff)
  })

# Test: rma.mh ----------------------------------------------------------------

test_that("Equal-Effects Model (OR) model works",
  {
    models_equal(
      rma.mh(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg),
      test_results$rma_mh_test_or)
  })
test_that("Equal-Effects Model (RR) model works",
  {
    models_equal(
      rma.mh(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg),
      test_results$rma_mh_test_rr)
  })

# Test: rma.peto ----------------------------------------------------------------

test_that("Equal-Effects Model (Peto's method) model works",
  {
    models_equal(
      rma.peto(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg),
      test_results$rma_peto_test)
  })

# Test: rma.glmm ----------------------------------------------------------------
test_that("Random-Effects Model (Unconditional Model with Fixed Study Effects) works",
  {
    models_equal(
      rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, model="UM.FS"),
      test_results$rma_glmm_umfs)
  })
test_that("Random-Effects Model (Unconditional Model with Random Study Effects) works",
  {
    models_equal(
      suppressWarnings(rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, 
        di=cneg, data=dat.bcg, model="UM.RS")),
      test_results$rma_glmm_umrs)
  })
test_that("Random-Effects Model (Conditional Model with Approximate Likelihood) works",
  {
    models_equal(
      rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, model="CM.AL"),
      test_results$rma_glmm_cmal)
  })


# Test: rma.mv ----------------------------------------------------------------

test_that("Multivariate Meta-Analysis Model (REML) works",
  {
    dat <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
    models_equal(
      rma.mv(yi, vi, random = ~ 1 | trial, data=dat),
      test_results$rma_mv)
  })
test_that("Multivariate Meta-Analysis Model (multilevel REML) works",
  {
    models_equal(
      rma.mv(yi, vi, random = ~ 1 | district/school, data=dat.konstantopoulos2011),
      test_results$rma_mv_mm)
  })
test_that("Multivariate Meta-Analysis Model (bivariate REML) works",
  {
    dat.long <- to.long(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
    levels(dat.long$group) <- c("exp", "con")
    dat.long$group <- relevel(dat.long$group, ref="con")
    dat.long <- escalc(measure="PLO", xi=out1, mi=out2, data=dat.long)
    models_equal(
      rma.mv(yi, vi, mods = ~ group, random = ~ group | study, struct="UN", data=dat.long),
      test_results$rma_mv_biv)
  })

# Test: confint.rma ----------------------------------------------------------------

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

test_that("RMA confidence intervals (uni) works",
  {
    models_equal(
      confint(res <- rma(yi, vi, data=dat, method="REML")),
      test_results$confint_rma_uni)
  })
test_that("RMA confidence intervals (mv) works",
  {
    models_equal(
      confint(rma.mv(yi, vi, random = ~ 1 | district/school, data=dat.konstantopoulos2011)),
      test_results$confint_rma_mv)
  })
res = rma.mv(yi, vi, random = ~ school | district, data=dat.konstantopoulos2011)
test_that("RMA confidence intervals (mv, parameterization) works",
  {
    models_equal(
      confint(res),
      test_results$confint_rma_mv_para)
  })
test_that("RMA confidence intervals (mv, single output) works",
  {
    models_equal(
      confint(res, tau2=1),
      test_results$confint_rma_mv_single)
  })
test_that("RMA confidence intervals (mv, custom CI level) works", {
  model <- confint(res, level = .8)
  
  tidy_model <- tidy_stats(model, args = .8)
  tidy_model_test <- test_results$confint_rma_mv_ci80
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
test_that("RMA confidence intervals (mh",
  {
    models_equal(
      confint(rma.mh(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)),
      test_results$confint_rma_mh)
  })

# Test: anova.rma ----------------------------------------------------------------

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
res1 <- rma(yi, vi, data=dat, method="ML")
res2 <- rma(yi, vi, mods = ~ ablat + year, data=dat, method="ML")
test_that("Wald-Type Tests for 'rma' Objects works",
  {
    models_equal(
      anova(res2),
      test_results$anova_rma_wald)
  })
test_that("Wald-Type Tests for 'rma' Objects (with moderators) works",
  {
    models_equal(
      anova(res2, X=rbind(c(0,1,0), c(0,0,1))),
      test_results$anova_rma_wald_est)
  })
test_that("Likelihood Ratio Tests for 'rma' Objects works",
  {
    models_equal(
      anova(res1, res2),
      test_results$anova_rma_lrt)
  })
test_that("Likelihood Ratio Tests (of linear combination) works",
  {
    models_equal(
      anova(res2, X=c(1,35,1970)),
      test_results$anova_rma_wald_comb)
  })
test_that("Likelihood Ratio Tests (for component) works",
  {
    dat <- dat.konstantopoulos2011
    res <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat)
    res0 <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat, sigma2=c(0,NA))
    models_equal(
      anova(res, res0),
      test_results$anova_rma_lrt_complex)
  })

# Test: permutest ----------------------------------------------------------------

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
test_that("Permutation Test for 'rma.uni' Objects works",
  {
    res <- rma(yi, vi, data=dat)
    set.seed(1234)
    models_equal(
      permutest(res, iter = 5, progbar = FALSE),
      test_results$permutest_single)
  })
test_that("Permutation Test for 'rma.uni' Objects (with moderators) works",
  {
    res <- rma(yi, vi, mods = ~ ablat + year, data=dat)
    set.seed(1234)
    models_equal(
      permutest(res, iter = 15, progbar = FALSE),
      test_results$permutest_mods)
  })
test_that("Permutation Test for 'rma.uni' Objects (for rma.ls list) works",
  {
    dat <- dat.bangertdrowns2004
    dat$ni100 <- dat$ni/100
    rma_uni_ls_sample <- rma(yi, vi, mods = ~ ni100, scale = ~ ni100, data=dat)
    set.seed(1234)
    models_equal(
      permutest(rma_uni_ls_sample, iter = 5, progbar = FALSE),
      test_results$permutest_ls)
  })

# Test: tes ----------------------------------------------------------------

test_that("Test of Excess Significance works",
  {
    dat <- escalc(measure="RR", ai=x.a, n1i=n.a, ci=x.p, n2i=n.p, data=dat.dorn2007)
    models_equal(
      tes(yi, vi, data=dat, test="chi2"),
      test_results$tes_result)
  })

# Test: matreg ----------------------------------------------------------------

test_that("Fit Regression Models (t statistic) works",
  {
    dat <- dat.craft2003
    tmp <- rcalc(ri ~ var1 + var2 | study, ni=ni, data=dat)
    V <- tmp$V
    dat <- tmp$dat
    dat$var1.var2 <- factor(dat$var1.var2,
      levels=c("acog.perf", "asom.perf", "conf.perf", "acog.asom", "acog.conf", "asom.conf"))
    res <- suppressWarnings(rma.mv(
      yi, V, mods = ~ var1.var2 - 1, random = ~ var1.var2 | study, struct="UN", data=dat))
    R <- vec2mat(coef(res))
    rownames(R) <- colnames(R) <- c("perf", "acog", "asom", "conf")
    models_equal(
      matreg(1, 2:4, R=R, V=vcov(res)),
      test_results$matreg_base)
  })

test_that("Fit Regression Models (z statistic) works",
  {
    dat.long <- to.long(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.colditz1994)
    dat.long <- escalc(measure="PLO", xi=out1, mi=out2, data=dat.long)
    dat.long$tpos <- dat.long$tneg <- dat.long$cpos <- dat.long$cneg <- NULL
    levels(dat.long$group) <- c("CON", "EXP")
    res <- rma.mv(yi, vi, mods = ~ group - 1, random = ~ group | trial, struct="UN",
                  data=dat.long, method="ML")
    models_equal(
      matreg(y=2, x=1, R=res$G, cov=TRUE, means=coef(res), n=res$g.levels.comb.k),
      test_results$matreg_biv)
  })

# Test: ranktest ----------------------------------------------------------------

test_that("Rank Correlation Test for Funnel Plot Asymmetry works",
  {
    dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
    models_equal(
      ranktest(yi, vi, data=dat),
      test_results$rank_test)
  })

# Test: regtest ----------------------------------------------------------------

dat <- dat.egger2001
dat <- escalc(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat, subset=-16)
test_that("Regression Test for Funnel Plot Asymmetry (weighted regression with multiplicative dispersion) works",
  {
    models_equal(
      regtest(yi, vi, data=dat, model="lm"),
      test_results$regtest_egge)
  })
test_that("Regression Test for Funnel Plot Asymmetry (mixed-effects meta-regression model) works",
  {
    models_equal(
      regtest(yi, vi, data=dat),
      test_results$regtest_mixed)
  })
test_that("Regression Test for Funnel Plot Asymmetry (mixed-effects meta-regression model with predictor specified) works",
  {
    models_equal(
      regtest(yi, vi, data=dat, predictor="ni"),
      test_results$regtest_pred)
  })

# Test: trimfill ----------------------------------------------------------------

test_that("Trim and Fill Analysis works",
  {
    dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
    res <- rma(yi, vi, data=dat, method="EE")
    models_equal(
      trimfill(res),
      test_results$trimfill_result)
  })


# Test: selmodel ----------------------------------------------------------------

test_that("Selection Model (beta; Random-Effects) works",
  {
    res <- rma(smd, se^2, data=dat.baskerville2012, method="ML", digits=3)
    models_equal(
      selmodel(res, type="beta"),
      test_results$selmodel_beta)
  })
test_that("Selection Model (half-normal; Equal-Effects) works",
  {
    dat <- escalc(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat.hahn2001, drop00=TRUE)
    res <- suppressWarnings(rma(yi, vi, data=dat, method="EE"))
    models_equal(
      selmodel(res, type="halfnorm", alternative="less"),
      test_results$selmodel_halfnorm)
  })
test_that("Selection Model (step function) works",
  {
    dat <- dat.hackshaw1998
    res <- rma(yi, vi, data=dat, method="ML")
    models_equal(
      selmodel(res, type="stepfun", alternative="greater", steps=c(.025,.10,.50,1)),
      test_results$selmodel_stepfun)
  })

# Test: fsn ----------------------------------------------------------------

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)


test_that("Fail-Safe N Analysis (Rosenthal approach) works",
  {
    models_equal(
      fsn(yi, vi, data=dat),
      test_results$fsn_rosenthal)
  })
test_that("Fail-Safe N Analysis (Orwin approach) works",
  {
    models_equal(
      fsn(yi, data=dat, type="Orwin", target=log(0.95)),
      test_results$fsn_orwin)
  })
test_that("Fail-Safe N Analysis (Rosenberg approach) works",
  {
    models_equal(
      fsn(yi, vi, data=dat, type="Rosenberg"),
      test_results$fsn_rosenberg)
  })


# Test: hc ----------------------------------------------------------------

dat <- escalc(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat.lee2004)
res <- rma(yi, vi, data=dat)

test_that("Henmi and Copas Meta-Analysis works",
  {
    models_equal(
      hc(res),
      test_results$hc_standard)
  })
test_that("Henmi and Copas Meta-Analysis (with transformation) works",
  {
    models_equal(
      hc(res, transf=exp),
      test_results$hc_tranformed)
  })


# Test: robust ----------------------------------------------------------------

test_that("Cluster-Robust Tests for Random-Effects Model works",
  {
    dat <- dat.bangertdrowns2004
    res <- rma(yi, vi, data=dat)
    models_equal(
      robust(res, cluster=id),
      test_results$robust_standard)
  })

test_that("Cluster-Robust Tests for Multivariate Meta-Analysis Model works",
  {
    dat <- dat.konstantopoulos2011
    res <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat)
    models_equal(
      robust(res, cluster=district),
      test_results$robust_multi_rem)
  })

test_that("Cluster-Robust Tests for Multivariate Meta-Analysis Model (multi-level) works",
  {
    dat <- dat.berkey1998
    V <- vcalc(vi=1, cluster=author, rvars=c(v1i, v2i), data=dat)
    res <- rma.mv(yi, V, mods = ~ outcome - 1, random = ~ outcome | trial, struct="UN", data=dat)
    models_equal(
      robust(res, cluster=trial),
      test_results$robust_mv)
  })

# Test: cumul ----------------------------------------------------------------

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
test_that("Cumulative Meta-Analysis (for Linear Models) works",
  {
    res <- rma(yi, vi, data=dat)
    models_equal(
      cumul(res, transf=exp, order=year),
      test_results$cumul_uni)
  })
test_that("Cumulative Meta-Analysis (for Multivariate Models) works",
  {
    res <- rma.mh(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
    models_equal(
      cumul(res, order=year),
      test_results$cumul_mh)
  })
