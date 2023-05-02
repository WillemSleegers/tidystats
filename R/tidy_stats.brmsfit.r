#' @describeIn tidy_stats tidy_stats method for class 'brmsfit'
tidy_stats.brmsfit <- function(x, args = NULL) {
  analysis <- list(method = "Bayesian regression model")

  priors <- FALSE
  prob <- .95
  robust <- FALSE
  mc_se <- FALSE
  if (!is.null(args)) {
    if (!is.null(args$priors)) {
      priors <- args$priors
    }
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

  summary <- summary(
    x,
    priors = priors, prob = prob, robust = robust, mc_se = mc_se
  )

  # Group-level effects
  if (length(summary$group) > 0) {
    group1 <- list(name = "Group-Level Effects")

    for (group in names(summary$random)) {
      group2 <- list(name = group)

      terms <- summary$random[[group]]

      for (i in seq_len(nrow(terms))) {
        group3 <- list(name = rownames(terms)[i])

        group3$statistics <- list() |>
          add_statistic(
            "estimate",
            terms$Estimate[i],
            "b",
            interval = "CI",
            level = prob,
            lower = terms[i, 3],
            upper = terms[i, 4]
          ) |>
          add_statistic("estimate error", terms$Est.Error[i], "EE") |>
          add_statistic("R-hat", terms$Rhat[i]) |>
          add_statistic(
            "effective sample size (bulk)",
            terms$Bulk_ESS[i],
            symbol = "ESS",
            subscript = "bulk"
          ) |>
          add_statistic(
            "effective sample size (tail)",
            terms$Tail_ESS[i],
            symbol = "ESS",
            subscript = "tail"
          )

        group2$groups <- append(group2$groups, list(group3))
      }
      group1$groups <- append(group1$groups, list(group2))
    }
    analysis$groups <- append(analysis$groups, list(group1))
  }

  # Population-level effects
  group1 <- list(name = "Population-Level Effects")

  terms <- summary$fixed

  for (i in seq_len(nrow(terms))) {
    group2 <- list(name = rownames(terms)[i])

    group2$statistics <- list() |>
      add_statistic(
        "estimate",
        terms$Estimate[i],
        "b",
        interval = "CI",
        level = prob,
        lower = terms[i, 3],
        upper = terms[i, 4]
      ) |>
      add_statistic("estimate error", terms$Est.Error[i], "EE") |>
      add_statistic("R-hat", terms$Rhat[i]) |>
      add_statistic(
        "effective sample size (bulk)",
        terms$Bulk_ESS[i],
        symbol = "ESS",
        subscript = "bulk"
      ) |>
      add_statistic(
        "effective sample size (tail)",
        terms$Tail_ESS[i],
        symbol = "ESS",
        subscript = "tail"
      )

    group1$groups <- append(group1$groups, list(group2))
  }
  analysis$groups <- append(analysis$groups, list(group1))


  analysis <- add_package_info(analysis, "brms")

  return(analysis)
}
