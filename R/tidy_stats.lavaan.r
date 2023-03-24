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
    add_statistic("n", x@Data@nobs[[1]]) |>
    add_statistic("k", x@Fit@npar) |>
    add_statistic("statistic", x@test$standard$stat, "χ²") |>
    add_statistic("df", x@test$standard$df) |>
    add_statistic("p", x@test$standard$pvalue)

  analysis$groups <- append(analysis$groups, list(group))

  # Fit statistics
  # if (!is.null(args$fit.measures)) {
  #   if (args$fit.measures) {
  #     fit <- summary$FIT

  #     statistics$CFI <- fit["cfi"]
  #     statistics$TLI <- fit["tli"]
  #     statistics$log_likelihood <- fit["logl"]
  #     statistics$log_likelihood_unrestricted <- fit["unrestricted.logl"]
  #     statistics$AIC <- fit["aic"]
  #     statistics$BIC <- fit["bic"]
  #     statistics$BIC_adjusted <- fit["bic2"]
  #     statistics$RMSEA <- fit["rmsea"]
  #     statistics$CI$CI_level <- .90
  #     statistics$CI$CI_lower <- fit["rmsea.ci.lower"]
  #     statistics$CI$CI_upper <- fit["rmsea.ci.upper"]
  #     statistics$p <- fit["rmsea.pvalue"]
  #     statistics$SRMR <- fit["srmr"]

  #     # Add baseline model
  #     model <- list()
  #     model$name <- "baseline model"
  #     model$statistics$statistic$name <- "X-squared"
  #     model$statistics$statistic$value <- fit["baseline.chisq"]
  #     model$statistics$df <- fit["baseline.df"]
  #     model$statistics$p <- fit["baseline.pvalue"]
  #     models[[2]] <- model
  #   }
  # }

  summary <- lavaan::summary(x)
  pe <- summary$pe

  # Latent Variables
  group_latent_vars <- list(name = "Latent variables")
  latent_vars <- dplyr::filter(pe, op == "=~")

  for (i in seq_len(nrow(latent_vars))) {
    group <- list(name = paste(latent_vars$lhs[i], "=~", latent_vars$rhs[i]))

    group$statistics <- list() |>
      add_statistic("estimate", latent_vars$est[i], "b") |>
      add_statistic("standard error", latent_vars$se[i], "SE") |>
      add_statistic("statistic", latent_vars$z[i], "z") |>
      add_statistic("p", latent_vars$z[i])

    group_latent_vars$groups <- append(group_latent_vars$groups, list(group))
  }

  analysis$groups <- append(analysis$groups, list(group_latent_vars))

  # Covariances
  group_covariances <- list(name = "Covariances")
  covariances <- dplyr::filter(pe, lhs != rhs & op == "~~")

  for (i in seq_len(nrow(covariances))) {
    group <- list(name = paste(covariances$lhs[i], "=~", covariances$rhs[i]))

    group$statistics <- list() |>
      add_statistic("estimate", latent_vars$est[i], "b") |>
      add_statistic("standard error", latent_vars$se[i], "SE") |>
      add_statistic("statistic", latent_vars$z[i], "z") |>
      add_statistic("p", latent_vars$z[i])

    group_covariances$groups <- append(group_covariances$groups, list(group))
  }

  analysis$groups <- append(analysis$groups, list(group_covariances))

  # Variances
  group_variances <- list(name = "Variances")
  variances <- dplyr::filter(pe, lhs == rhs & op == "~~")

  for (i in seq_len(nrow(variances))) {
    group <- list(name = paste(variances$lhs[i]))

    group$statistics <- list() |>
      add_statistic("estimate", latent_vars$est[i], "b") |>
      add_statistic("standard error", latent_vars$se[i], "SE") |>
      add_statistic("statistic", latent_vars$z[i], "z") |>
      add_statistic("p", latent_vars$z[i])

    group_variances$groups <- append(group_variances$groups, list(group))
  }

  analysis$groups <- append(analysis$groups, list(group_variances))

  # Additional information
  analysis$estimator <- summary$optim$estimator

  return(analysis)
}
