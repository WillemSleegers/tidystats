#' @describeIn tidy_stats tidy_stats method for class 'brmsfit'
#' @export
tidy_stats.brmsfit <- function(x, args = NULL) {
  analysis <- list(method = "Bayesian regression model")

  prob <- .95
  robust <- FALSE
  mc_se <- FALSE

  if (!is.null(args)) {
    if (!is.null(args$prob)) {
      prob <- args$prob
    }
    if (!is.null(args$robust)) {
      robust <- args$robust
    }
    if (!is.null(args$mc_se)) {
      mc_se <- args$mc_se
    }
  }

  summary <- summary(x, prob = prob, robust = robust, mc_se = mc_se)

  # Group-level effects
  if (length(summary$group) > 0) {
    group1 <- list(name = "Group-Level Effects")

    for (group in names(summary$random)) {
      group2 <- list(name = group)

      group2 <- add_brms_statistics(
        summary$random[[group]], group2,
        prob = prob
      )
      group1$groups <- append(group1$groups, list(group2))
    }
    analysis$groups <- append(analysis$groups, list(group1))
  }

  # Population-level effects
  group <- list(name = "Population-Level Effects")

  group <- add_brms_statistics(summary$fixed, group, prob = prob)
  analysis$groups <- append(analysis$groups, list(group))

  # Family specific parameters
  if (nrow(summary$spec_pars) > 0) {
    group <- list(name = "Family Specific Parameters")

    group <- add_brms_statistics(summary$spec_pars, group, prob = prob)
    analysis$groups <- append(analysis$groups, list(group))
  }

  analysis <- add_package_info(analysis, "brms")

  # Extra information
  analysis$robust <- robust

  return(analysis)
}

add_brms_statistics <- function(x, list, prob) {
  for (i in seq_len(nrow(x))) {
    group <- list(name = rownames(x)[i])

    group$statistics <- list() |>
      add_statistic(
        "estimate",
        x$Estimate[i],
        "b",
        interval = "CI",
        level = prob,
        lower = x[i, paste0("l-", round(prob * 100), "% CI")],
        upper = x[i, paste0("u-", round(prob * 100), "% CI")]
      ) |>
      add_statistic("Monte Carlo standard error", x$MCSE[i], "MCSE") |>
      add_statistic("estimate error", x$Est.Error[i], "EE") |>
      add_statistic("R-hat", x$Rhat[i], symbol = symbol("R_hat")) |>
      add_statistic(
        "effective sample size (bulk)",
        x$Bulk_ESS[i],
        symbol = "ESS",
        subscript = "bulk"
      ) |>
      add_statistic(
        "effective sample size (tail)",
        x$Tail_ESS[i],
        symbol = "ESS",
        subscript = "tail"
      )

    list$groups <- append(list$groups, list(group))
  }

  return(list)
}
