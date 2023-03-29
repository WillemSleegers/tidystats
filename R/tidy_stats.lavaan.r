#' @describeIn tidy_stats tidy_stats method for class 'lavaan'
tidy_stats.lavaan <- function(x, args = NULL) {
  analysis <- list()

  if (x@Options$model.type == "cfa") {
    analysis$method <- "Confirmatory factor analysis"
  } else {
    analysis$method <- "Structual equation model"
  }

  # Model Test User Model
  group <- list(name = "Model Test User Model")

  group$statistics <- list() |>
    add_statistic("k", x@Fit@npar) |>
    add_statistic("n", sum(unlist(x@Data@nobs))) |>
    add_statistic("statistic", x@test$standard$stat, "χ²") |>
    add_statistic("df", x@test$standard$df) |>
    add_statistic("p", x@test$standard$pvalue)

  analysis$groups <- append(analysis$groups, list(group))

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
        "p", fit[["rmsea.pvalue"]],
        subscript = "RMSEA <= 0.050"
      ) |>
      add_statistic(
        "p", fit[["rmsea.notclose.pvalue"]],
        subscript = "RMSEA >= 0.080"
      ) |>
      add_statistic(
        "Standardized root mean square residual", fit[["srmr"]],
        symbol = "SRMR"
      )

    analysis$groups <- append(analysis$groups, list(group))
  }

  if (x@Data@ngroups > 1) {
    group_groups <- list(name = "Groups")

    for (g in 1:x@Data@ngroups) {
      group <- list(name = x@Data@group.label[g])

      # Model test user model
      group_model <- list(name = "Model Test User Model")

      group_model$statistics <- list() |>
        add_statistic("n", x@Data@nobs[[g]]) |>
        add_statistic("statistic", x@test$standard$stat.group[[g]], "χ²")

      group$groups <- append(group$groups, list(group_model))

      # Parameter estimates
      group <- add_parameter_estimates(group, summary$pe)
      group_groups$groups <- append(group_groups$groups, list(group))
    }

    analysis$groups <- append(analysis$groups, list(group_groups))
  } else {
    analysis <- add_parameter_estimates(analysis, summary$pe)
  }

  # Additional information
  analysis$estimator <- summary$optim$estimator

  return(analysis)
}

add_parameter_estimates <- function(list, pe) {
  # Latent Variables
  group_latent_vars <- list(name = "Latent variables")
  latent_vars <- dplyr::filter(pe, op == "=~")

  for (i in seq_len(nrow(latent_vars))) {
    group <- list(name = paste(latent_vars$lhs[i], "=~", latent_vars$rhs[i]))

    group$statistics <- list() |>
      add_statistic("estimate", latent_vars$est[i], "b") |>
      add_statistic("standard error", latent_vars$se[i], "SE") |>
      add_statistic("statistic", latent_vars$z[i], "z") |>
      add_statistic("p", latent_vars$p[i]) |>
      add_statistic(
        "estimate", latent_vars$std.lv[i],
        symbol = "β", subscript = "lv"
      ) |>
      add_statistic(
        "estimate", latent_vars$std.all[i],
        symbol = "β", subscript = "all"
      )

    group_latent_vars$groups <- append(group_latent_vars$groups, list(group))
  }

  list$groups <- append(list$groups, list(group_latent_vars))

  # Covariances
  group_covariances <- list(name = "Covariances")
  covariances <- dplyr::filter(pe, lhs != rhs & op == "~~")

  for (i in seq_len(nrow(covariances))) {
    group <- list(name = paste(covariances$lhs[i], "=~", covariances$rhs[i]))

    group$statistics <- list() |>
      add_statistic("estimate", covariances$est[i], "b") |>
      add_statistic("standard error", covariances$se[i], "SE") |>
      add_statistic("statistic", covariances$z[i], "z") |>
      add_statistic("p", covariances$p[i]) |>
      add_statistic(
        "estimate", covariances$std.lv[i],
        symbol = "β", subscript = "lv"
      ) |>
      add_statistic(
        "estimate", covariances$std.all[i],
        symbol = "β", subscript = "all"
      )

    group_covariances$groups <- append(group_covariances$groups, list(group))
  }

  list$groups <- append(list$groups, list(group_covariances))

  # Intercepts
  if ("~1" %in% pe$op) {
    group_intercepts <- list(name = "Intercepts")
    intercepts <- dplyr::filter(pe, op == "~1")

    for (i in seq_len(nrow(intercepts))) {
      group <- list(name = paste(intercepts$lhs[i]))

      group$statistics <- list() |>
        add_statistic("estimate", intercepts$est[i], "b") |>
        add_statistic("standard error", intercepts$se[i], "SE") |>
        add_statistic("statistic", intercepts$z[i], "z") |>
        add_statistic("p", intercepts$p[i]) |>
        add_statistic(
          "estimate", intercepts$std.lv[i],
          symbol = "β", subscript = "lv"
        ) |>
        add_statistic(
          "estimate", intercepts$std.all[i],
          symbol = "β", subscript = "all"
        )

      group_intercepts$groups <- append(group_intercepts$groups, list(group))
    }

    list$groups <- append(list$groups, list(group_intercepts))
  }

  # Variances
  group_variances <- list(name = "Variances")
  variances <- dplyr::filter(pe, lhs == rhs & op == "~~")

  for (i in seq_len(nrow(variances))) {
    group <- list(name = paste(variances$lhs[i]))

    group$statistics <- list() |>
      add_statistic("estimate", variances$est[i], "b") |>
      add_statistic("standard error", variances$se[i], "SE") |>
      add_statistic("statistic", variances$z[i], "z") |>
      add_statistic("p", variances$p[i]) |>
      add_statistic(
        "estimate", variances$std.lv[i],
        symbol = "β", subscript = "lv"
      ) |>
      add_statistic(
        "estimate", variances$std.all[i],
        symbol = "β", subscript = "all"
      )

    group_variances$groups <- append(group_variances$groups, list(group))
  }

  list$groups <- append(list$groups, list(group_variances))

  return(list)
}
