#' @describeIn tidy_stats tidy_stats method for class 'lmerMod'
#' @export
tidy_stats.lmerMod <- function(x, args = NULL) {
  analysis <- list(
    name = deparse(attr(x@frame, "formula")),
    method = "Linear mixed model"
  )

  summary <- summary(x)

  # Model
  group_model <- list(name = "Model")
  statistics <- list()

  if ("AIC" %in% names(summary$AICtab)) {
    statistics <- add_statistic(statistics, "AIC", summary$AICtab[["AIC"]])
    statistics <- add_statistic(statistics, "BIC", summary$AICtab[["BIC"]])
    statistics <- add_statistic(
      statistics,
      "log likelihood",
      summary$AICtab[["logLik"]],
      "l"
    )
    statistics <- add_statistic(
      statistics,
      "-2*log(L)",
      summary$AICtab[["-2*log(L)"]]
    )
    statistics <- add_statistic(
      statistics,
      "residual df",
      summary$AICtab[["df.resid"]],
      "df",
      "res."
    )
  }

  # Add number of observations
  statistics <- add_statistic(statistics, "N", summary[[3]]$dims[[1]])

  # Add statistics to the model's statistics
  group_model$statistics <- statistics

  # Add the group to a groups element on the analysis
  analysis$groups <- append(analysis$groups, list(group_model))

  # Random effects
  group_RE <- list(name = "Random effects")

  # Get variance-covariance matrix
  varcor <- summary$varcor

  # Loop over the groups
  for (i in seq_along(length(varcor))) {
    # Create lists for the group
    group_RE_group <- list(name = names(varcor)[i])
    group_RE_variances <- list(name = "Variances")

    # Set N for the group, if there is an N
    if (names(varcor)[i] %in% names(summary$ngrps)) {
      statistics <- list()
      statistics <- add_statistic(
        statistics,
        "N",
        summary$ngrps[names(summary$ngrps) == names(varcor)[i]][[1]]
      )

      group_RE_group$statistics <- statistics
    }

    # Extract standard deviations
    SDs <- attr(varcor[[i]], "stddev")

    # Loop over variances in the group
    for (j in seq_along(length(SDs))) {
      group_RE_group_variance <- list(name = names(SDs)[j])

      # Extract statistics
      statistics <- list()

      statistics <- add_statistic(
        statistics,
        "standard deviation",
        SDs[[j]],
        "SD"
      )
      statistics <- add_statistic(statistics, "variance", SDs[[j]]^2, "var")

      # Add statistics to the RE group
      group_RE_group_variance$statistics <- statistics

      # Add RE group variances to the RE group
      group_RE_variances$groups <- append(
        group_RE_variances$groups,
        list(group_RE_group_variance)
      )
    }

    # Add variances to the group
    group_RE_group$groups <- append(
      group_RE_group$groups,
      list(group_RE_variances)
    )

    # Extract correlations
    cors <- attr(varcor[[i]], "correlation")

    # Check if there are any correlations
    if (length(cors) > 1) {
      # Create a group for the correlations
      group_RE_correlations <- list(name = "Correlations")

      # Tidy the matrix
      cors <- tidy_matrix(cors)

      # Loop over the correlations
      for (j in seq_len(nrow(cors))) {
        names <- list(
          list(name = cors$name1[i]),
          list(name = cors$name2[i])
        )
        group_RE_correlation <- list(names = names)

        statistics <- list()
        statistics <- add_statistic(
          statistics,
          "correlation",
          cors$value[j],
          "r"
        )

        group_RE_correlation$statistics <- statistics

        group_RE_correlations$groups <- append(
          group_RE_correlations$groups,
          list(group_RE_correlation)
        )
      }

      # Add correlations to the group
      group_RE_group$groups <- append(
        group_RE_group$groups,
        list(group_RE_correlations)
      )
    }

    # Add group to the groups list
    group_RE$groups <- append(
      group_RE$groups,
      list(group_RE_group)
    )
  }

  # Add the Residual group
  group_RE_group <- list(name = "Residual")

  statistics <- list()
  statistics <- add_statistic(
    statistics,
    "standard deviation",
    attr(varcor, "sc"),
    "SD"
  )
  statistics <- add_statistic(
    statistics,
    "variance",
    attr(varcor, "sc")^2,
    "var"
  )

  group_RE_group$statistics <- statistics

  group_RE$groups <- append(
    group_RE$groups,
    list(group_RE_group)
  )

  # Add RE groups to the statistics element on the analysis
  analysis$groups <- append(analysis$groups, list(group_RE))

  # Fixed effects
  # Create a groups lists for the random effects, terms, and pairs
  group_FE <- list(name = "Fixed effects")
  group_FE_coefficients <- list(name = "Coefficients")
  group_FE_pairs <- list(name = "Correlations")

  # Get coefficient statistics
  coefs <- stats::coef(summary)

  # Loop over the coefficients
  for (i in seq_len(nrow(coefs))) {
    # Create a list for the coefficient and set the name
    group_FE_coefficient <- list(name = rownames(coefs)[i])

    # Create a new statistics list and add the fixed effect's statistics
    statistics <- list()

    statistics <- add_statistic(
      statistics,
      "estimate",
      coefs[i, "Estimate"],
      "b"
    )
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std. Error"])
    statistics <- add_statistic(
      statistics,
      "statistic",
      coefs[i, "t value"],
      "t"
    )

    # Add statistics to the coefficient list
    group_FE_coefficient$statistics <- statistics

    # Add coefficient to the groups of coefficients
    group_FE_coefficients$groups <- append(
      group_FE_coefficients$groups,
      list(group_FE_coefficient)
    )
  }

  # Add group of coefficients to the fixed effect group
  group_FE$groups <- append(group_FE$groups, list(group_FE_coefficients))

  # Extract fixed correlations
  fixed_cors <- attr(summary$vcov, "factors")$correlation

  # Check if there are any correlations
  if (length(fixed_cors) > 1) {
    # Tidy the matrix
    fixed_cors <- tidy_matrix(fixed_cors)

    # Loop over the pairs
    for (i in seq_len(nrow(fixed_cors))) {
      names <- list(
        list(name = fixed_cors$name1[i]),
        list(name = fixed_cors$name2[i])
      )
      group_FE_pair <- list(names = names)

      statistics <- list()
      statistics <- add_statistic(
        statistics,
        "correlation",
        fixed_cors$value[i],
        "r"
      )

      group_FE_pair$statistics <- statistics

      group_FE_pairs$groups <- append(
        group_FE_pairs$groups,
        list(group_FE_pair)
      )
    }

    # Add FE pairs to the statistics of the FE group
    group_FE$groups <- append(group_FE$groups, list(group_FE_pairs))
  }

  # Add FE groups to the statistics element on the analysis
  analysis$groups <- append(analysis$groups, list(group_FE))

  # Extract REML criterion at convergence
  if ("REML" %in% names(summary$AICtab)) {
    analysis$REML_criterion_at_convergence <- summary$AICtab[[1]]
  }

  # Add additional convergence information
  analysis$convergence_code <- summary$optinfo$conv$opt
  analysis$convergence_message <- summary$optinfo$conv$lme4$messages

  # Add package information
  analysis <- add_package_info(analysis, "lme4")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'lmerModLmerTest'
#' @export
tidy_stats.lmerModLmerTest <- function(x, args = NULL) {
  analysis <- list(
    name = paste(
      stringr::str_replace_all(
        string = deparse(attr(x@frame, "formula")),
        pattern = "^ +",
        replacement = ""
      ),
      collapse = ""
    ),
    method = "Linear mixed model"
  )

  summary <- summary(x)

  # Model fit
  group_model <- list(name = "Model")
  statistics <- list()

  # Check if the AIC is in the AICtab, if so, add the statistics
  if ("AIC" %in% names(summary$AICtab)) {
    statistics <- statistics |>
      add_statistic("AIC", summary$AICtab[["AIC"]]) |>
      add_statistic("BIC", summary$AICtab[["BIC"]]) |>
      add_statistic("log likelihood", summary$AICtab[["logLik"]], "l") |>
      add_statistic("-2*log(L)", summary$AICtab[["-2*log(L)"]]) |>
      add_statistic("residual df", summary$AICtab[["df.resid"]], "df", "res.")
  }

  # Add number of observations
  statistics <- add_statistic(statistics, "N", summary[[3]]$dims[[1]])

  group_model$statistics <- statistics
  analysis$groups <- append(analysis$groups, list(group_model))

  # Random effects
  group_RE <- list(name = "Random effects")

  varcor <- summary$varcor

  for (i in seq_len(length(varcor))) {
    group_RE_group <- list(name = names(varcor)[i])
    group_RE_variances <- list(name = "Variances")

    # Set N for the group, if there is an N
    if (names(varcor)[i] %in% names(summary$ngrps)) {
      statistics <- list()
      statistics <- add_statistic(
        statistics,
        "N",
        summary$ngrps[names(summary$ngrps) == names(varcor)[i]][[1]]
      )

      group_RE_group$statistics <- statistics
    }

    # Extract standard deviations
    SDs <- attr(varcor[[i]], "stddev")

    # Loop over variances in the group
    for (j in seq_along(length(SDs))) {
      group_RE_group_variance <- list(name = names(SDs)[j])

      statistics <- list() |>
        add_statistic("standard deviation", SDs[[j]], "SD") |>
        add_statistic("variance", SDs[[j]]^2, "var")

      group_RE_group_variance$statistics <- statistics
      group_RE_variances$groups <- append(
        group_RE_variances$groups,
        list(group_RE_group_variance)
      )
    }

    group_RE_group$groups <- append(
      group_RE_group$groups,
      list(group_RE_variances)
    )

    cors <- attr(varcor[[i]], "correlation")

    if (length(cors) > 1) {
      group_RE_correlations <- list(name = "Correlations")

      cors <- tidy_matrix(cors)

      for (j in seq_len(nrow(cors))) {
        names <- list(
          list(name = cors$name1[i]),
          list(name = cors$name2[i])
        )
        group_RE_correlation <- list(names = names)

        statistics <- list()
        statistics <- add_statistic(
          statistics,
          "correlation",
          cors$value[j],
          "r"
        )

        group_RE_correlation$statistics <- statistics

        group_RE_correlations$groups <- append(
          group_RE_correlations$groups,
          list(group_RE_correlation)
        )
      }

      # Add correlations to the group
      group_RE_group$groups <- append(
        group_RE_group$groups,
        list(group_RE_correlations)
      )
    }
    # Add group to the groups list
    group_RE$groups <- append(
      group_RE$groups,
      list(group_RE_group)
    )
  }

  # Add the Residual group
  group_RE_group <- list(name = "Residual")

  statistics <- list()
  statistics <- add_statistic(
    statistics,
    "standard deviation",
    attr(varcor, "sc"),
    "SD"
  )
  statistics <- add_statistic(
    statistics,
    "variance",
    attr(varcor, "sc")^2,
    "var"
  )

  group_RE_group$statistics <- statistics

  group_RE$groups <- append(
    group_RE$groups,
    list(group_RE_group)
  )

  # Add RE groups to the statistics element on the analysis
  analysis$groups <- append(analysis$groups, list(group_RE))

  # Fixed effects
  # Create a groups lists for the random effects, terms, and pairs
  group_FE <- list(name = "Fixed effects")
  group_FE_coefficients <- list(name = "Coefficients")
  group_FE_pairs <- list(name = "Correlations")

  # Get coefficient statistics
  coefs <- stats::coef(summary)

  # Loop over the coefficients
  for (i in seq_len(nrow(coefs))) {
    # Create a list for the coefficient and set the name
    group_FE_coefficient <- list(name = rownames(coefs)[i])

    # Create a new statistics list and add the fixed effect's statistics
    statistics <- list()

    statistics <- add_statistic(
      statistics,
      "estimate",
      coefs[i, "Estimate"],
      "b"
    )
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std. Error"])
    statistics <- add_statistic(statistics, "df", coefs[i, "df"])
    statistics <- add_statistic(
      statistics,
      "statistic",
      coefs[i, "t value"],
      "t"
    )
    statistics <- add_statistic(statistics, "p", coefs[i, "Pr(>|t|)"])

    # Add statistics to the coefficient list
    group_FE_coefficient$statistics <- statistics

    # Add coefficient to the groups of coefficients
    group_FE_coefficients$groups <- append(
      group_FE_coefficients$groups,
      list(group_FE_coefficient)
    )
  }

  # Add group of coefficients to the fixed effect group
  group_FE$groups <- append(group_FE$groups, list(group_FE_coefficients))

  # Extract fixed correlations
  fixed_cors <- attr(summary$vcov, "factors")$correlation

  # Check if there are any correlations
  if (length(fixed_cors) > 1) {
    # Tidy the matrix
    fixed_cors <- tidy_matrix(fixed_cors)

    # Loop over the pairs
    for (i in seq_len(nrow(fixed_cors))) {
      names <- list(
        list(name = fixed_cors$name1[i]),
        list(name = fixed_cors$name2[i])
      )
      group_FE_pair <- list(names = names)

      statistics <- list()
      statistics <- add_statistic(
        statistics,
        "correlation",
        fixed_cors$value[i],
        "r"
      )

      group_FE_pair$statistics <- statistics

      group_FE_pairs$groups <- append(
        group_FE_pairs$groups,
        list(group_FE_pair)
      )
    }

    # Add FE pairs to the statistics of the FE group
    group_FE$groups <- append(group_FE$groups, list(group_FE_pairs))
  }

  # Add FE groups to the statistics element on the analysis
  analysis$groups <- append(analysis$groups, list(group_FE))

  # Extract REML criterion at convergence
  if ("REML" %in% names(summary$AICtab)) {
    analysis$REML_criterion_at_convergence <- summary$AICtab[[1]]
  }

  # Add additional convergence information
  analysis$convergence_code <- summary$optinfo$conv$opt
  analysis$convergence_message <- summary$optinfo$conv$lme4$messages

  # Add package information
  analysis <- add_package_info(analysis, "lmerTest")

  return(analysis)
}
