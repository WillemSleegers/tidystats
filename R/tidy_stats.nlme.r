#' @describeIn tidy_stats tidy_stats method for class 'lme'
#' @export
tidy_stats.lme <- function(x, args = NULL) {
  analysis <- list(
    method = "Linear mixed-effects model"
  )

  summary <- summary(x)

  # Model
  group <- list(name = "Model")
  group$statistics <- list() |>
    add_statistic("N", x$dims$N) |>
    add_statistic("AIC", summary$AIC) |>
    add_statistic("BIC", summary$BIC) |>
    add_statistic("log likelihood", summary$logLik, "l") |>
    add_statistic("sigma", summary$sigma, "s", "res.")

  analysis$groups <- append(analysis$groups, list(group))

  # Random effects
  group_RE <- list(name = "Random effects")
  group_variances <- list(name = "Variances")
  group_RE_pairs <- list(name = "Correlations")

  RE <- x$modelStruct$reStruct

  for (i in 1:x$dims$Q) {
    groups <- list(name = names(RE)[i])

    varcor <- nlme::VarCorr(RE[[i]], sigma = x$sigma)
    corr <- attr(varcor, "corr")

    for (j in seq_len(nrow(varcor))) {
      group <- list(name = rownames(varcor)[j])

      group$statistics <- list() |>
        add_statistic("var", varcor[j, "Variance"]) |>
        add_statistic("SD", varcor[j, "StdDev"])

      groups$groups <- append(groups$groups, list(group))
    }

    group_variances$groups <- append(group_variances$groups, list(groups))

    if (!is.null(corr)) {
      pairs <- combn(rownames(varcor), 2)
      for (j in seq_len(ncol(pairs))) {

      }
    }
  }

  group <- list(name = "Residuals")
  group$statistics <- list() |>
    add_statistic("variance", x$sigma^2) |>
    add_statistic("SD", x$sigma)

  group_variances$groups <- append(group_variances$groups, list(group))
  group_RE$groups <- append(group_RE$groups, list(group_variances))

  # Random effects - Pairs




  for (i in 1:x$dims$Q) {
    groups <- list(name = names(RE)[i])

    varcor <- nlme::VarCorr(RE[[i]], sigma = x$sigma)

    for (j in seq_len(nrow(varcor))) {
      group <- list(name = rownames(varcor)[j])

      group$statistics <- list() |>
        add_statistic("var", varcor[j, "Variance"]) |>
        add_statistic("SD", varcor[j, "StdDev"])

      groups$groups <- append(groups$groups, list(group))
    }

    group_variances$groups <- append(group_variances$groups, list(groups))
  }



  # Extract correlations, if there are any
  if (nrow(corrs) > 0) {
    # Loop over the groups
    for (group_name in unique(dplyr::pull(corrs, group))) {
      # Create a list for the group
      group_RE_pairs <- list(name = group_name)

      # Subset the corrs data from to only the current group
      group_corrs <- dplyr::filter(corrs, group == group_name)

      # Tidy the data frame so that each row is a correlation pair
      pairs <- group_corrs %>%
        dplyr::rename(name2 = name) %>%
        pivot_longer(cols = c(-group, -name2), names_to = "name1")

      # Loop over the pairs
      for (i in 1:nrow(pairs)) {
        names <- list(
          list(name = pairs$name1[i]),
          list(name = pairs$name2[i])
        )
        group_RE_pair <- list(names = names)

        statistics <- list()
        statistics <- add_statistic(
          statistics, "correlation", pairs$value[i],
          "r"
        )

        group_RE_pair$statistics <- statistics
      }

      group_RE_pairs$groups <- append(
        group_RE_pairs$groups,
        list(group_RE_pair)
      )
    }
  }

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
  for (i in 1:nrow(coefs)) {
    # Create a list for the coefficient and set the name
    group_FE_coefficient <- list(name = rownames(coefs)[i])

    # Create a new statistics list and add the fixed effect's statistics
    statistics <- list()

    statistics <- add_statistic(
      statistics, "estimate", coefs[i, "Value"],
      "b"
    )
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std.Error"])
    statistics <- add_statistic(statistics, "df", coefs[i, "DF"])
    statistics <- add_statistic(
      statistics, "statistic", coefs[i, "t-value"],
      "t"
    )
    statistics <- add_statistic(statistics, "p", coefs[i, "p-value"])

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
  fixed_cors <- summary$corFixed

  # Check if there are any correlations
  if (length(fixed_cors) > 1) {
    # Tidy the matrix
    fixed_cors <- tidy_matrix(fixed_cors)

    # Loop over the pairs
    for (i in 1:nrow(fixed_cors)) {
      names <- list(
        list(name = fixed_cors$name1[i]),
        list(name = fixed_cors$name2[i])
      )
      group_FE_pair <- list(names = names)

      statistics <- list()
      statistics <- add_statistic(
        statistics, "correlation",
        fixed_cors$value[i], "r"
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

  # Add package information
  analysis <- add_package_info(analysis, "nlme")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'nlme'
#' @export
tidy_stats.nlme <- function(x, args = NULL) {
  # Create a list to store the analysis in and set the method
  analysis <- list(
    method = "Nonlinear mixed-effects model"
  )

  # Get summary statistics
  summary <- summary(x)

  # Model fit
  # Create a group and statistics list for the model fit statistics
  group_model <- list(name = "Model")
  statistics <- list()

  statistics <- add_statistic(statistics, "N", x$dims$N)
  statistics <- add_statistic(statistics, "AIC", summary$AIC)
  statistics <- add_statistic(statistics, "BIC", summary$BIC)
  statistics <- add_statistic(statistics, "log likelihood", summary$logLik, "l")
  statistics <- add_statistic(statistics, "sigma", summary$sigma, "s", "res.")

  # Add statistics to the model's statistics
  group_model$statistics <- statistics

  # Add the group to a groups element on the analysis
  analysis$groups <- append(analysis$groups, list(group_model))

  # Random effects
  # Create a groups lists for the random effects, terms, and pairs
  group_RE <- list(name = "Random effects")
  group_RE_groups <- list(name = "Groups")
  group_RE_pairs <- list(name = "Correlations")

  # Get variance-covariance matrix
  varcor <- VarCorr(x)

  # Convert the matrix to a data frame with the variances and one with the
  # correlations
  coefs <- tibble()
  corrs <- tibble()
  group_row <- 0
  for (i in 1:nrow(varcor)) {
    rowname <- rownames(varcor)[i]

    if (x$dims$Q == 1) {
      group <- attr(varcor, "title")
    } else if (str_detect(rowname, " =")) {
      group <- paste(rowname, varcor[i, "Variance"])
      group_row <- i
    }

    if (rowname == "Residual") {
      coefs <- bind_rows(coefs, tibble(
        group = "Residual",
        var = varcor[i, "Variance"], sd = varcor[i, "StdDev"]
      ))
    } else {
      coefs <- bind_rows(coefs, tibble(
        group = group, coef = rowname,
        var = varcor[i, "Variance"], sd = varcor[i, "StdDev"]
      ))

      if ("Corr" %in% colnames(varcor)) {
        if (varcor[i, "Corr"] != "") {
          value <- suppressWarnings(as.numeric(varcor[i, "Corr"]))
          if (is.na(value)) {
            rownames(varcor)[]

            colnames <- rownames(varcor)[(group_row + 1):(group_row +
              length(colnames(varcor)) - 2)]
          } else {
            c(rownames(varcor)[i], varcor[i, 3:length(colnames(varcor))])

            corrs <- suppressMessages(rbind(
              corrs,
              c(
                group, rownames(varcor)[i],
                varcor[i, 3:length(colnames(varcor))]
              )
            ))
          }
        }
      }
    }
  }
  colnames(corrs) <- c("group", "name", colnames)

  # Loop over the coefficient groups
  for (group_name in unique(dplyr::pull(coefs, group))) {
    # Create a list for the group
    group_RE_group <- list(name = group_name)

    # Set N for the group, if this isn't the Residuals group
    if (group_name != "Residual") {
      # Set N for the group
      statistics <- list()

      statistics <- add_statistic(
        statistics, "N",
        x$dims$ngrps[stringr::str_detect(group_name, names(x$dims$ngrps))][[1]]
      )

      group_RE_group$statistics <- statistics

      # Subset the coefs data from to only the current group
      group_coefs <- dplyr::filter(coefs, group == group_name)

      for (i in 1:nrow(group_coefs)) {
        group_RE_group_coefficient <- list(name = group_coefs$coef[i])

        # Extract statistics
        statistics <- list()

        statistics <- add_statistic(
          statistics, "standard deviation",
          group_coefs$sd[i], "SD"
        )
        statistics <- add_statistic(
          statistics, "variance", group_coefs$var[i],
          "var"
        )

        # Add statistics to the RE group
        group_RE_group_coefficient$statistics <- statistics

        # Add RE group coefficients to the RE group
        group_RE_group$groups <- append(
          group_RE_group$groups,
          list(group_RE_group_coefficient)
        )
      }
    } else {
      # Create a statistics list for the Residual statistics
      statistics <- list()

      statistics <- add_statistic(
        statistics, "standard deviation",
        as.numeric(varcor[i, "StdDev"]), "SD"
      )
      statistics <- add_statistic(
        statistics, "variance",
        as.numeric(varcor[i, "Variance"]), "var"
      )

      group_RE_group$statistics <- statistics
    }

    # Add RE group to the RE groups list
    group_RE_groups$groups <- append(
      group_RE_groups$groups,
      list(group_RE_group)
    )
  }

  # Extract correlations, if there are any
  if (nrow(corrs) > 0) {
    # Loop over the groups
    for (group_name in unique(dplyr::pull(corrs, group))) {
      # Create a list for the group
      group_RE_pairs <- list(name = group_name)

      # Subset the corrs data from to only the current group
      group_corrs <- dplyr::filter(corrs, group == group_name)

      # Tidy the data frame so that each row is a correlation pair
      pairs <- group_corrs %>%
        dplyr::rename(name2 = name) %>%
        pivot_longer(cols = c(-group, -name2), names_to = "name1")

      # Loop over the pairs
      for (i in 1:nrow(pairs)) {
        names <- list(
          list(name = pairs$name1[i]),
          list(name = pairs$name2[i])
        )
        group_RE_pair <- list(names = names)

        statistics <- list()
        statistics <- add_statistic(
          statistics, "correlation", pairs$value[i],
          "r"
        )

        group_RE_pair$statistics <- statistics
      }

      group_RE_pairs$groups <- append(
        group_RE_pairs$groups,
        list(group_RE_pair)
      )
    }
  }

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
  for (i in 1:nrow(coefs)) {
    # Create a list for the coefficient and set the name
    group_FE_coefficient <- list(name = rownames(coefs)[i])

    # Create a new statistics list and add the fixed effect's statistics
    statistics <- list()

    statistics <- add_statistic(
      statistics, "estimate", coefs[i, "Value"],
      "b"
    )
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std.Error"])
    statistics <- add_statistic(statistics, "df", coefs[i, "DF"])
    statistics <- add_statistic(
      statistics, "statistic", coefs[i, "t-value"],
      "t"
    )
    statistics <- add_statistic(statistics, "p", coefs[i, "p-value"])

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
  fixed_cors <- summary$corFixed

  # Check if there are any correlations
  if (length(fixed_cors) > 1) {
    # Tidy the matrix
    fixed_cors <- tidy_matrix(fixed_cors)

    # Loop over the pairs
    for (i in 1:nrow(fixed_cors)) {
      names <- list(
        list(name = fixed_cors$name1[i]),
        list(name = fixed_cors$name2[i])
      )
      group_FE_pair <- list(names = names)

      statistics <- list()
      statistics <- add_statistic(
        statistics, "correlation",
        fixed_cors$value[i], "r"
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

  # Add package information
  analysis <- add_package_info(analysis, "nlme")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'anova.lme'
#' @export
tidy_stats.anova.lme <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()

  # Set method
  analysis$method <- "ANOVA"

  # Create a groups list to add model or term statistics to
  groups <- list(name = dplyr::if_else("Model" %in% colnames(x), "Models",
    "Terms"
  ))

  # Loop over the models or terms
  for (i in 1:length(rownames(x))) {
    # Create a group list and set the name
    group <- list(name = rownames(x)[i])

    # Create a statistics list and add statistics
    statistics <- list()

    statistics <- add_statistic(statistics, "AIC", x$AIC[i])
    statistics <- add_statistic(statistics, "BIC", x$BIC[i])
    statistics <- add_statistic(statistics, "log likelihood", x$logLik[i], "l")
    statistics <- add_statistic(
      statistics, "likelihood ratio", x$L.Ratio[i],
      "LR"
    )
    statistics <- add_statistic(statistics, "df", x$df[i])
    statistics <- add_statistic(
      statistics, "df numerator", x$NumDF[i], "df",
      "num."
    )
    statistics <- add_statistic(
      statistics, "df denominator", x$DenDF[i], "df",
      "den."
    )
    statistics <- add_statistic(statistics, "statistic", x$`F-value`[i], "F")
    statistics <- add_statistic(statistics, "p", x$p - value[i])

    # Add the statistics to the group
    group$statistics <- statistics

    # Add the group to the groups list
    groups$groups <- append(groups$groups, list(group))
  }

  # Add the groups to the analysis list
  analysis$groups <- append(analysis$groups, list(groups))

  # Add package information
  analysis <- add_package_info(analysis, "nlme")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'gls'
#' @export
tidy_stats.gls <- function(x, args = NULL) {
  # Create the analysis list and set the method
  analysis <- list(method = "Linear model using generalized least squares")

  # Get summary statistics
  summary <- summary(x)

  # Model fit
  # Create a group and statistics list for the model fit statistics
  group_model <- list(name = "Model")
  statistics <- list()

  statistics <- add_statistic(statistics, "N", x$dims$N)
  statistics <- add_statistic(statistics, "AIC", summary$AIC)
  statistics <- add_statistic(statistics, "BIC", summary$BIC)
  statistics <- add_statistic(statistics, "log likelihood", summary$logLik, "l")
  statistics <- add_statistic(statistics, "sigma", summary$sigma, "s", "res.")

  # Add statistics to the model's statistics
  group_model$statistics <- statistics

  # Add the group to a groups element on the analysis
  analysis$groups <- append(analysis$groups, list(group_model))

  # Create lists for the coefficients and correlations
  group_coefficients <- list(name = "Coefficients")
  group_correlations <- list(name = "Correlations")

  # Get coefficient statistics
  coefs <- stats::coef(summary)

  # Loop over the coefficients
  for (i in 1:nrow(coefs)) {
    # Create a list for the coefficient and set the name
    group_coefficient <- list(name = rownames(coefs)[i])

    # Create a new statistics list and add the fixed effect's statistics
    statistics <- list()

    statistics <- add_statistic(
      statistics, "estimate", coefs[i, "Value"],
      "b"
    )
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std.Error"])
    statistics <- add_statistic(
      statistics, "statistic", coefs[i, "t-value"],
      "t"
    )
    statistics <- add_statistic(statistics, "p", coefs[i, "p-value"])

    # Add statistics to the coefficient list
    group_coefficient$statistics <- statistics

    # Add coefficient to the groups of coefficients
    group_coefficients$groups <- append(
      group_coefficients$groups,
      list(group_coefficient)
    )
  }

  # Add group of coefficients to the analysis group
  analysis$groups <- append(analysis$groups, list(group_coefficients))

  # Extract correlations
  cors <- summary$corBeta

  # Check if there are any correlations
  if (length(cors) > 1) {
    # Tidy the matrix
    cors <- tidy_matrix(cors)

    # Loop over the pairs
    for (i in 1:nrow(fixed_cors)) {
      names <- list(
        list(name = cors$name1[i]),
        list(name = cors$name2[i])
      )
      group_correlation <- list(names = names)

      statistics <- list()
      statistics <- add_statistic(
        statistics, "correlation",
        cors$value[i], "r"
      )

      group_correlation$statistics <- statistics

      group_correlations$groups <- append(
        group_correlations$groups,
        list(group_correlation)
      )
    }
  }

  # Add group of correlations to the analysis groups
  analysis$groups <- append(analysis$groups, list(group_correlations))

  # Add package information
  analysis <- add_package_info(analysis, "nlme")

  return(analysis)
}
