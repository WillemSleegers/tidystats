#' @describeIn tidy_stats tidy_stats method for class 'lavaan'
#' @export
tidy_stats.lavaan <- function(x, args = NULL) {
  analysis <- list()

  if (x@Options$model.type == "cfa") {
    analysis$method <- "Confirmatory factor analysis"
  } else {
    analysis$method <- "Structual equation model"
  }

  fit_measures <- TRUE
  standardized <- TRUE
  if (!is.null(args)) {
    if (!is.null(args$fit.measures)) {
      fit_measures <- args$fit.measures
    }

    if (!is.null(args$standardized)) {
      standardized <- args$standardized
    }
  }

  summary <- lavaan::summary(
    x,
    fit.measures = fit_measures, standardized = standardized
  )

  # Model Test User Model
  group <- list(name = "Model Test User Model")

  statistics <- add_statistic(list(), "k", x@Fit@npar)

  if (methods::slot(x@Model, "multilevel")) {
    statistics <- statistics |>
      add_statistic(
        "observations", x@Data@Lp[[1]]$nclusters[[1]],
        symbol = "n"
      ) |>
      add_statistic("clusters", x@Data@Lp[[1]]$nclusters[[2]],
        symbol = "n", subscript = "cluster"
      )
  } else {
    statistics <- add_statistic(
      statistics,
      "observations", sum(unlist(x@Data@nobs)), "n"
    )
  }

  statistics <- statistics |>
    add_statistic(
      "statistic",
      x@test$standard$stat,
      paste0(intToUtf8(0x03c7), intToUtf8(0x00b2))
    ) |>
    add_statistic("df", x@test$standard$df) |>
    add_statistic("p", x@test$standard$pvalue)

  group$statistics <- statistics

  analysis$groups <- append(analysis$groups, list(group))

  # Fit measures
  if (fit_measures) {
    group <- list(name = "Fit measures")

    fit <- summary$fit

    group$statistics <- list() |>
      add_statistic("Comparative fit index", fit[["cfi"]], symbol = "CFI") |>
      add_statistic("Tucker-Lewis Index", fit[["tli"]], symbol = "TLI") |>
      add_statistic(
        "Loglikelihood user model", fit[["logl"]],
        symbol = "l", subscript = "H0"
      ) |>
      add_statistic(
        "Loglikelihood unrestricted model",
        fit[["unrestricted.logl"]],
        symbol = "l", subscript = "H1"
      ) |>
      add_statistic(
        "Akaike information criterion", fit[["aic"]],
        symbol = "AIC"
      ) |>
      add_statistic(
        "Bayesian information criterion", fit[["bic"]],
        symbol = "BIC"
      ) |>
      add_statistic(
        "Sample-size adjusted Bayesian information criterion", fit[["bic2"]],
        symbol = "SABIC"
      ) |>
      add_statistic(
        "Root mean square error of approximation", fit[["rmsea"]],
        symbol = "RMSEA",
        interval = "CI", level = fit[["rmsea.ci.level"]],
        lower = fit[["rmsea.ci.lower"]], upper = fit[["rmsea.ci.upper"]]
      ) |>
      add_statistic(
        "p (RMSEA <= 0.050)", fit[["rmsea.pvalue"]],
        symbol = "p",
        subscript = "RMSEA <= 0.050"
      ) |>
      add_statistic(
        "p (RMSEA >= 0.080)", fit[["rmsea.notclose.pvalue"]],
        symbol = "p", subscript = "RMSEA >= 0.080"
      ) |>
      add_statistic(
        "Standardized root mean square residual", fit[["srmr"]],
        symbol = "SRMR"
      )

    analysis$groups <- append(analysis$groups, list(group))
  }

  if (methods::slot(x@Model, "multilevel")) {
    group_levels <- list(name = "Levels")

    for (l in 1:x@Data@nlevels) {
      group <- list(name = x@Data@block.label[l])

      level_pe <- dplyr::filter(summary$pe, level == l)

      if (x@Data@ngroups > 1) {
        group <- add_group(group, x, level_pe)
      } else {
        group <- add_parameter_estimates(group, level_pe)
      }
      group_levels$groups <- append(group_levels$groups, list(group))
    }
    analysis$groups <- append(analysis$groups, list(group_levels))
  } else {
    if (x@Data@ngroups > 1) {
      analysis <- add_group(analysis, x, summary$pe)
    } else {
      analysis <- add_parameter_estimates(analysis, summary$pe)
    }
  }

  # Additional information
  analysis$estimator <- summary$optim$estimator

  return(analysis)
}

add_group <- function(list, x, pe) {
  group_groups <- list(name = "Groups")

  for (g in 1:x@Data@ngroups) {
    group <- list(name = x@Data@group.label[g])

    # Model test user model
    group_model <- list(name = "Model Test User Model")

    group_model$statistics <- list() |>
      add_statistic("n", x@Data@nobs[[g]]) |>
      add_statistic(
        "statistic",
        x@test$standard$stat.group[[g]],
        paste0(intToUtf8(0x03c7), intToUtf8(0x00b2))
      )

    group$groups <- append(group$groups, list(group_model))

    # Parameter estimates
    group_pe <- dplyr::filter(pe, group == g)
    group <- add_parameter_estimates(group, group_pe)
    group_groups$groups <- append(group_groups$groups, list(group))
  }

  list$groups <- append(list$groups, list(group_groups))

  return(list)
}

add_parameter_estimates <- function(list, pe) {
  # Latent variables
  latent_vars <- dplyr::filter(pe, op == "=~")

  if (nrow(latent_vars) > 0) {
    list <- add_latent_vars(list, latent_vars)
  }

  # Regressions
  regressions <- dplyr::filter(pe, op == "~")

  if (nrow(regressions) > 0) {
    list <- add_regressions(list, regressions)
  }

  # Covariances
  covariances <- dplyr::filter(pe, lhs != rhs & op == "~~")

  if (nrow(covariances) > 0) {
    list <- add_covariances(list, covariances)
  }

  # Intercepts
  intercepts <- dplyr::filter(pe, op == "~1")

  if (nrow(intercepts) > 0) {
    list <- add_intercepts(list, intercepts)
  }

  # Variances
  variances <- dplyr::filter(pe, lhs == rhs & op == "~~")

  if (nrow(variances) > 0) {
    list <- add_variances(list, variances)
  }

  # Defined parameters
  def_pars <- dplyr::filter(pe, op == ":=")

  if (nrow(def_pars) > 0) {
    list <- add_defined_parameters(list, def_pars)
  }

  return(list)
}

add_latent_vars <- function(list, pe) {
  groups <- list(name = "Latent variables")

  for (i in seq_len(nrow(pe))) {
    group <- list(name = paste(pe$lhs[i], "=~", pe$rhs[i]))

    group$statistics <- list() |>
      add_statistic("estimate", pe$est[i], "b") |>
      add_statistic("standard error", pe$se[i], "SE") |>
      add_statistic("statistic", pe$z[i], "z") |>
      add_statistic("p", pe$p[i]) |>
      add_statistic(
        "standardized estimate (lv)", pe$std.lv[i],
        symbol = intToUtf8(0x03b2), subscript = "lv"
      ) |>
      add_statistic(
        "standardized estimate (all)", pe$std.all[i],
        symbol = intToUtf8(0x03b2), subscript = "all"
      )

    groups$groups <- append(groups$groups, list(group))
  }

  list$groups <- append(list$groups, list(groups))

  return(list)
}

add_regressions <- function(list, pe) {
  groups <- list(name = "Regressions")

  for (i in seq_len(nrow(pe))) {
    group <- list(name = paste(pe$lhs[i], "~", pe$rhs[i]))

    group$statistics <- list() |>
      add_statistic("estimate", pe$est[i], "b") |>
      add_statistic("standard error", pe$se[i], "SE") |>
      add_statistic("statistic", pe$z[i], "z") |>
      add_statistic("p", pe$p[i]) |>
      add_statistic(
        "standardized estimate (lv)", pe$std.lv[i],
        symbol = intToUtf8(0x03b2), subscript = "lv"
      ) |>
      add_statistic(
        "standardized estimate (all)", pe$std.all[i],
        symbol = intToUtf8(0x03b2), subscript = "all"
      )

    groups$groups <- append(groups$groups, list(group))
  }

  list$groups <- append(list$groups, list(groups))

  return(list)
}

add_covariances <- function(list, pe) {
  groups <- list(name = "Covariances")

  for (i in seq_len(nrow(pe))) {
    group <- list(name = paste(pe$lhs[i], "~~", pe$rhs[i]))

    group$statistics <- list() |>
      add_statistic("estimate", pe$est[i], "b") |>
      add_statistic("standard error", pe$se[i], "SE") |>
      add_statistic("statistic", pe$z[i], "z") |>
      add_statistic("p", pe$p[i]) |>
      add_statistic(
        "standardized estimate (lv)", pe$std.lv[i],
        symbol = intToUtf8(0x03b2), subscript = "lv"
      ) |>
      add_statistic(
        "standardized estimate (all)", pe$std.all[i],
        symbol = intToUtf8(0x03b2), subscript = "all"
      )

    groups$groups <- append(groups$groups, list(group))
  }

  list$groups <- append(list$groups, list(groups))

  return(list)
}

add_intercepts <- function(list, pe) {
  groups <- list(name = "Intercepts")

  for (i in seq_len(nrow(pe))) {
    group <- list(name = paste(pe$lhs[i]))

    group$statistics <- list() |>
      add_statistic("estimate", pe$est[i], "b") |>
      add_statistic("standard error", pe$se[i], "SE") |>
      add_statistic("statistic", pe$z[i], "z") |>
      add_statistic("p", pe$p[i]) |>
      add_statistic(
        "standardized estimate (lv)", pe$std.lv[i],
        symbol = intToUtf8(0x03b2), subscript = "lv"
      ) |>
      add_statistic(
        "standardized estimate (all)", pe$std.all[i],
        symbol = intToUtf8(0x03b2), subscript = "all"
      )

    groups$groups <- append(groups$groups, list(group))
  }

  list$groups <- append(list$groups, list(groups))

  return(list)
}

add_variances <- function(list, pe) {
  groups <- list(name = "Variances")

  for (i in seq_len(nrow(pe))) {
    group <- list(name = paste(pe$lhs[i]))

    group$statistics <- list() |>
      add_statistic("estimate", pe$est[i], "b") |>
      add_statistic("standard error", pe$se[i], "SE") |>
      add_statistic("statistic", pe$z[i], "z") |>
      add_statistic("p", pe$p[i]) |>
      add_statistic(
        "standardized estimate (lv)", pe$std.lv[i],
        symbol = intToUtf8(0x03b2), subscript = "lv"
      ) |>
      add_statistic(
        "standardized estimate (all)", pe$std.all[i],
        symbol = intToUtf8(0x03b2), subscript = "all"
      )

    groups$groups <- append(groups$groups, list(group))
  }

  list$groups <- append(list$groups, list(groups))

  return(list)
}

add_defined_parameters <- function(list, pe) {
  groups <- list(name = "Defined parameters")

  for (i in seq_len(nrow(pe))) {
    group <- list(name = paste(pe$lhs[i], "~", pe$rhs[i]))

    group$statistics <- list() |>
      add_statistic("estimate", pe$est[i], "b") |>
      add_statistic("standard error", pe$se[i], "SE") |>
      add_statistic("statistic", pe$z[i], "z") |>
      add_statistic("p", pe$p[i]) |>
      add_statistic(
        "standardized estimate (lv)", pe$std.lv[i],
        symbol = intToUtf8(0x03b2), subscript = "lv"
      ) |>
      add_statistic(
        "standardized estimate (all)", pe$std.all[i],
        symbol = intToUtf8(0x03b2), subscript = "all"
      )

    groups$groups <- append(groups$groups, list(group))
  }

  list$groups <- append(list$groups, list(groups))

  return(list)
}
