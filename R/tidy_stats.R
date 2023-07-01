#' Tidy the output of a statistics object
#'
#' \code{tidy_stats} is used to convert the output of a statistical object to a
#' list of organized statistics. The \code{tidy_stats} function is automatically
#' run when \code{add_stats} is used, so there is generally no need to use this
#' function explicitly. It can be used, however, to peek at how the output of a
#' specific analysis will be organized.
#'
#' Please note that not all statistical tests are supported. See 'Details'
#' below for a list of supported statistical tests.
#'
#' @param x The output of a statistical test.
#'
#' @details
#' Currently supported functions:
#'
#' \code{stats}:
#' \itemize{
#'   \item \code{t.test()}
#'   \item \code{cor.test()}
#'   \item \code{chisq.test()}
#'   \item \code{wilcox.test()}
#'   \item \code{fisher.test()}
#'   \item \code{oneway.test()}
#'   \item \code{lm()}
#'   \item \code{glm()}
#'   \item \code{aov()}
#'   \item \code{anova()}
#' }
#'
#' \code{lme4}/\code{lmerTest}:
#' \itemize{
#'   \item \code{lmer()}
#' }
#'
#' \code{BayesFactor}:
#' \itemize{
#'   \item \code{generalTestBF()}
#'   \item \code{lmBF()}
#'   \item \code{regressionBF()}
#'   \item \code{ttestBF()}
#'   \item \code{anovaBF()}
#'   \item \code{correlationBF()}
#'   \item \code{contingencyTableBF()}
#'   \item \code{proportionBF()}
#'   \item \code{meta.ttestBF()}
#' }
#'
#' \code{tidystats}:
#' \itemize{
#'   \item \code{describe_data()}
#'   \item \code{count_data()}
#' }
#'
#' @examples
#' # Conduct statistical tests
#' # t-test:
#' sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)
#'
#' # lm:
#' ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
#' trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
#' weight <- c(ctl, trt)
#' lm_D9 <- lm(weight ~ group)
#'
#' # ANOVA:
#' npk_aov <- aov(yield ~ block + N * P * K, npk)
#'
#' # Tidy the statistics and store each analysis in a separate variable
#' list_sleep_test <- tidy_stats(sleep_test)
#' list_lm_D9 <- tidy_stats(lm_D9)
#' list_npk_aov <- tidy_stats(npk_aov)
#'
#' # Now you can inspect each of these variables, e.g.,:
#' names(list_sleep_test)
#' str(list_sleep_test)
#'
#' @keywords internal
tidy_stats <- function(x, args = NULL) UseMethod("tidy_stats")

#' @describeIn tidy_stats tidy_stats method for class 'htest'
#' @keywords internal
tidy_stats.htest <- function(x, args = NULL) {
  analysis <- list()

  # Set the analysis name
  # Some names contain additional parameters; we remove those and potentially
  # store them separately
  name <- x$data.name

  name <- stringr::str_remove(name, pattern = " ,\n using scores: .*")
  name <- stringr::str_remove(name, pattern = ", null probability .*")
  name <- stringr::str_remove(name, pattern = " time base: .*")

  analysis <- list(name = as.character(name))

  # Set the analysis method
  # Special case: Mauchly's test of sphericity has multiple method values
  x$method <- x$method[[1]]
  method <- x$method

  method <- trimws(method) # To remove a space in the Two Sample t-test
  method <- stringr::str_remove(method, " with Yates' continuity correction")
  method <- stringr::str_remove(method, " for given probabilities")
  method <- stringr::str_remove(method, " with continuity correction")
  method <- stringr::str_remove(method, " without continuity correction")
  method <- stringr::str_remove(method, " with simulated p-value\n.*")
  method <- stringr::str_remove(method, " hybrid using asym\\.chisq\\. iff .*")
  method <- stringr::str_remove(method, " \\(not assuming equal variances\\)")
  method <- stringr::str_remove(method, " in [0-9]+ x [0-9] x k tables")

  analysis$method <- method

  # Create a list to add the statistics to
  statistics <- list()

  # Set the estimate, if there is one
  if (!is.null(x$estimate)) {
    # Set the value of the estimate
    # - Special case: Calculate the estimate as a mean difference in the case of
    #   a two sample t-test
    # - Special case: If there is more than 1 estimate, set the value to NA
    if (length(x$estimate) > 1) {
      if (stringr::str_detect(method, "Two Sample t-test")) {
        value <- x$estimate[[1]] - x$estimate[[2]]
      } else {
        value <- NA
      }
    } else {
      value <- x$estimate[[1]]
    }

    # Explicitly ask for the first element because sometimes there are more, in
    # which case case_when() returns multiple values
    symbol <- dplyr::case_when(
      names(x$estimate)[1] == "cor" ~ "r",
      names(x$estimate)[1] == "tau" ~ "r",
      names(x$estimate)[1] == "rho" ~ "r",
      names(x$estimate)[1] == "odds ratio" ~ "OR",
      names(x$estimate)[1] == "p" ~ "p̂",
      names(x$estimate)[1] == "difference in location" ~ "Mdn",
      names(x$estimate)[1] == "ratio of variances" ~ "VR",
      names(x$estimate)[1] == "probability of success" ~ "p",
      names(x$estimate)[1] == "ratio of scales" ~ "s",
      names(x$estimate)[1] == "event rate" ~ "λ",
      names(x$estimate)[1] == "rate ratio" ~ "RR",
      names(x$estimate)[1] == "common odds ratio" ~ "OR",
      stringr::str_detect(method, "t-test") ~ "M"
    )

    subscript <- dplyr::case_when(
      stringr::str_detect(method, "Two Sample t-test") |
        method == "Paired t-test" ~ "diff.",
      names(x$estimate)[1] == "tau" ~ "τ",
      names(x$estimate)[1] == "rho" ~ "S",
      names(x$estimate)[1] == "difference in location" ~ "diff.",
      names(x$estimate)[1] == "probability of success" ~ "success"
    )

    # Add the estimate
    statistics <- add_statistic(
      statistics, "estimate", value, symbol,
      subscript, "CI", attr(x$conf.int, "conf.level"), x$conf.int[1],
      x$conf.int[2]
    )
  }

  # Add the standard error
  statistics <- add_statistic(statistics, "SE", x$stderr)

  # Set the statistic
  if (!is.null(names(x$statistic))) {
    value <- x$statistic[[1]]

    symbol <- dplyr::case_when(
      names(x$statistic) == "X-squared" ~ "χ²",
      names(x$statistic) == "Kruskal-Wallis chi-squared" ~ "χ²",
      names(x$statistic) == "D^+" ~ "D",
      names(x$statistic) == "D^-" ~ "D",
      stringr::str_detect(names(x$statistic), "McNemar") ~ "χ²",
      names(x$statistic) == "Quade F" ~ "F",
      names(x$statistic) == "Bartlett's K-squared" ~ "K²",
      names(x$statistic) == "Fligner-Killeen:med chi-squared" ~ "χ²",
      names(x$statistic) == "number of successes" ~ "k",
      names(x$statistic) == "number of events" ~ "n",
      names(x$statistic) == "count1" ~ "n",
      names(x$statistic) == "Friedman chi-squared" ~ "χ²",
      names(x$statistic) == "Cochran-Mantel-Haenszel M^2" ~ "CMH",
      names(x$statistic) == "Mantel-Haenszel X-squared" ~ "χ²",
      names(x$statistic) == "Dickey-Fuller" ~ "DF",
      TRUE ~ names(x$statistic)
    )

    if (names(x$statistic) == "Dickey-Fuller") {
      subscript <- "τ"
    } else {
      subscript <- NA
    }

    name <- "statistic"

    statistics <- add_statistic(statistics, name, value, symbol, subscript)
  }

  # Set the parameter, if there is one/are any
  if (!is.null(x$parameter)) {
    # Special case: Sometimes there's both a numerator and denominator df
    if (length(x$parameter) > 1) {
      statistics <- add_statistic(
        statistics,
        "df numerator",
        x$parameter[[1]],
        "df",
        "num."
      )
      statistics <- add_statistic(
        statistics,
        "df denominator",
        x$parameter[[2]],
        "df",
        "den."
      )
    } else {
      value <- x$parameter[[1]]

      # Various special cases because not all parameters are degrees of freedom
      subscript <- NA
      symbol <- NA

      if (method == "Phillips-Perron Unit Root Test") {
        name <- "truncation lag"
        symbol <- "k"
      } else if (method == "Exact binomial test") {
        name <- "number of trials"
        symbol <- "n"
      } else if (method == "Exact Poisson test") {
        name <- "time base"
        symbol <- "T"
      } else if (method == "Comparison of Poisson rates") {
        name <- "expected count"
        symbol <- "n"
        subscript <- "expected"
      } else {
        name <- "df"
      }

      statistics <- add_statistic(statistics, name, value, symbol, subscript)
    }
  }

  # Set the p-value
  statistics <- add_statistic(statistics, "p", x$p.value)

  # Add statistics to the analysis
  analysis$statistics <- statistics

  # Add additional information
  # Information about the alternative hypothesis
  if (!is.null(x$alternative)) {
    alternative <- list(direction = x$alternative)

    if (!is.null(x$null.value)) {
      alternative$null_value <- x$null.value[[1]]
    }

    analysis$alternative <- alternative
  }

  # Number of simulations if the p-value was simulated
  if (stringr::str_detect(x$method, "simulated p-value")) {
    analysis$sim <- as.numeric(stringr::str_extract(
      x$method,
      "[0-9](e\\+)?([0-9].)?"
    ))
  }

  # Hybrid parameters
  if (stringr::str_detect(x$method, "hybrid")) {
    analysis$hybrid_parameters <- list(
      expect = readr::parse_number(
        stringr::str_extract(x$method, "exp=[0-9+]")
      ),
      percent = readr::parse_number(stringr::str_extract(
        x$method,
        "perc=[0-9]+"
      )),
      Emin = readr::parse_number(stringr::str_extract(x$method, "Emin=[0-9+]"))
    )
  }

  # Whether the variance was assumed to be equal
  if (x$method == "Welch Two Sample t-test") {
    analysis$var_equal <- FALSE
  } else if (x$method == " Two Sample t-test") {
    analysis$var_equal <- TRUE
  } else if (x$method == "One-way analysis of means") {
    analysis$var_equal <- TRUE
  } else if (stringr::str_detect(
    x$method,
    "\\(not assuming equal variances\\)"
  )) {
    analysis$var_equal <- FALSE
  }

  # Add package information
  analysis <- add_package_info(analysis, "stats")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'pairwise.htest'
tidy_stats.pairwise.htest <- function(x, args = NULL) {
  # Create the analysis list and set the name
  analysis <- list(name = x$data.name)

  # Add method to the analysis
  analysis$method <- dplyr::if_else(
    stringr::str_starts(x$method, "Pairwise"),
    x$method,
    paste("Pairwise", x$method)
  )

  # Check if there is 1 or more terms
  # If 1, only create a statistics list
  # If multiple, loop over terms and create separate lists for each term
  if (nrow(x$p.value) == 1) {
    statistics <- list()
    statistics <- add_statistic(statistics, "p", x$p.value[1])
    analysis$statistics <- statistics
  } else {
    groups <- list(name = "Pairs")

    p_values <- tidy_matrix(x$p.value, symmetric = FALSE)

    for (i in seq_len(nrow(p_values))) {
      names <- list(
        list(name = p_values$name1[i]),
        list(name = p_values$name2[i])
      )
      group <- list(names = names)

      statistics <- list()
      statistics <- add_statistic(statistics, "p", p_values$value[i])

      group$statistics <- statistics
      groups$groups <- append(groups$groups, list(group))
    }

    analysis$groups <- append(analysis$groups, list(groups))
  }

  # Add additional information
  analysis$p_adjust_method <- x$p.adjust.method

  # Add package information
  analysis <- add_package_info(analysis, "stats")

  return(analysis)
}


#' @describeIn tidy_stats tidy_stats method for class 'lm'
#' @keywords internal
tidy_stats.lm <- function(x, args = NULL) {
  analysis <- list(
    name = deparse(x$call[[2]]),
    method = "Linear regression"
  )

  summary <- summary(x)

  # Model fit
  group <- list(name = "Model")
  group$statistics <- list() |>
    add_statistic("R squared", summary$r.squared, "R²") |>
    add_statistic("adj. R squared", summary$adj.r.squared, "R²", "adj.") |>
    add_statistic("statistic", summary$fstatistic[[1]], "F") |>
    add_statistic("df numerator", summary$fstatistic[[2]], "df", "num.") |>
    add_statistic("df denominator", summary$fstatistic[[3]], "df", "den.") |>
    add_statistic(
      "p",
      stats::pf(
        summary$fstatistic[[1]],
        summary$fstatistic[[2]],
        summary$fstatistic[[3]],
        lower.tail = FALSE
      )
    ) |>
    add_statistic("sigma", summary$sigma, "s", "res.")

  analysis$groups <- append(analysis$groups, list(group))

  # Create a groups list for the coefficients
  groups <- list(name = "Coefficients")

  # Extract statistics of the coefficients
  coefs <- stats::coef(summary)

  # Loop over the coefficients and add statistics to a group list
  for (i in seq_len(nrow(coefs))) {
    # Create a new group list
    group <- list()

    # Add the name and type of the coefficient
    group$name <- rownames(coefs)[i]

    # Create a new statistics list
    statistics <- list()

    statistics <- add_statistic(
      statistics, "estimate", coefs[i, "Estimate"],
      "b"
    )
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std. Error"])
    statistics <- add_statistic(
      statistics, "statistic", coefs[i, "t value"],
      "t"
    )
    statistics <- add_statistic(statistics, "df", summary$df[2])
    statistics <- add_statistic(statistics, "p", coefs[i, "Pr(>|t|)"])

    # Add statistics to the group
    group$statistics <- statistics

    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }

  # Add the coefficient groups to the statistics list
  analysis$groups <- append(analysis$groups, list(groups))

  # Add package information
  analysis <- add_package_info(analysis, "stats")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'glm'
tidy_stats.glm <- function(x, args = NULL) {
  # Create the analysis list and set the name and method
  analysis <- list(
    name = deparse(x$call[[2]]),
    method = "Generalized linear regression"
  )

  # Get summary statistics
  summary <- summary(x)

  # Model fit
  # Create a group and statistics list for the model fit statistics
  group <- list(name = "Model")
  statistics <- list()

  # Extract and add statistics to the statistics list
  statistics <- add_statistic(
    statistics, "null deviance",
    summary$null.deviance, "D", "null"
  )
  statistics <- add_statistic(
    statistics, "residual deviance", summary$deviance,
    "D", "res."
  )
  statistics <- add_statistic(
    statistics, "null df", summary$df.null, "df",
    "null"
  )
  statistics <- add_statistic(
    statistics, "residual df", summary$df.residual,
    "df", "res."
  )
  statistics <- add_statistic(statistics, "AIC", summary$aic)

  # Add statistics to the model group
  group$statistics <- statistics

  # Add the group to a statistics element on the analysis
  analysis$groups <- append(analysis$groups, list(group))

  # Create a (new) groups list for the coefficients
  groups <- list(name = "Coefficients")

  # Extract statistics of the coefficients
  coefs <- stats::coef(summary)

  # Loop over the coefficients and add statistics to a group list
  for (i in seq_len(nrow(coefs))) {
    # Create a new group list
    group <- list()

    # Add the name and type of the coefficient
    group$name <- rownames(coefs)[i]

    # Create a new statistics list
    statistics <- list()

    statistics <- add_statistic(
      statistics, "estimate", coefs[i, "Estimate"],
      "b"
    )
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std. Error"])
    statistics <- add_statistic(
      statistics, "statistic", coefs[i, 3],
      dplyr::if_else(colnames(coefs)[3] == "z value", "z", "t")
    )
    statistics <- add_statistic(statistics, "df", summary$df[2])
    statistics <- add_statistic(statistics, "p", coefs[i, 4])

    # Add statistics to the group
    group$statistics <- statistics

    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }

  # Add the coefficient groups to the statistics list
  analysis$groups <- append(analysis$groups, list(groups))

  # Add additional information
  analysis$family <- x$family$family
  analysis$link <- x$family$link

  if (!is.null(summary$dispersion)) {
    analysis$dispersion <- summary$dispersion
  }

  if (!is.null(summary$fisher_scoring_iterations)) {
    analysis$iterations <- summary$iter
  }

  # Add package information
  analysis <- add_package_info(analysis, "stats")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'anova'
tidy_stats.anova <- function(x, args = NULL) {
  analysis <- list()

  # Extract the heading to figure out the type of ANOVA and name
  heading <- attr(x, "heading")

  if (stringr::str_detect(heading[1], "Analysis of Deviance")) {
    method <- "ANODE"
  } else {
    method <- "ANOVA"
  }

  # Determine and set the name, if there is one
  if (sum(stringr::str_detect(heading, "Response: ")) > 0) {
    if (length(heading) == 1) {
      analysis$name <- paste(
        stringr::str_extract(heading, "(?<=Response: ).*"), " ~ ",
        paste(rownames(x)[-1], collapse = " + ")
      )
    } else {
      analysis$name <- paste(
        stringr::str_extract(heading[2], "(?<=Response: ).*"), " ~ ",
        paste(rownames(x)[-length(x)], collapse = " + ")
      )
    }
  }

  # Check whether multiple models are being compared
  if (sum(stringr::str_detect(heading, "Models:|Model 1:")) > 0) {
    model_comparison <- TRUE
  } else {
    model_comparison <- FALSE
  }

  # Set the method
  analysis$method <- method

  # Trim spaces from the rownames
  rownames(x) <- stringr::str_trim(rownames(x))

  # Replace the numeric names with model names in case of a model comparison
  # ANOVA
  if (model_comparison) {
    x$name <- stringr::str_remove(
      unlist(stringr::str_split(
        heading[2], "\n"
      )), "Model [0-9+]: "
    )
  }

  # Create an empty groups list to add model or term statistics to
  groups <- list(name = dplyr::if_else(model_comparison, "Models", "Terms"))

  # Loop over each row
  for (i in seq_len(nrow(x))) {
    # Create a new group list
    group <- list()

    # Set the name
    if (!is.null(rownames(x)[i])) {
      group$name <- rownames(x)[i]
    } else if (!is.null(x$name[i])) {
      group$name <- x$name[i] # TODO: Check if this one is necessary
    }

    # Create a new statistics list and add statistics
    statistics <- list()

    statistics <- statistics |>
      add_statistic("n parameters", x$npar[i], "k") |>
      add_statistic("AIC", x$AIC[i]) |>
      add_statistic("BIC", x$BIC[i]) |>
      add_statistic("log likelihood", x$logLik[i], "l") |>
      add_statistic("deviance", x$deviance[i], "D") |>
      add_statistic("deviance", x$Deviance[i], "D") |>
      add_statistic("residual deviance", x$`Resid. Dev`[i], "D", "res.") |>
      add_statistic("RSS", x$RSS[i]) |>
      add_statistic("SS", x$`Sum Sq`[i]) |>
      add_statistic("SS", x$`Sum of Sq`[i]) |>
      add_statistic("MS", x$`Mean Sq`[i]) |>
      add_statistic("statistic", x$Chisq[i], "χ²") |>
      add_statistic("statistic", x[i, "F"], "F") |>
      add_statistic("statistic", x$`F value`[i], "F")

    # Special case: Degrees of freedom
    if (rownames(x)[length(rownames(x))] == "Residuals") {
      if (i == length(rownames(x)) && rownames(x)[i] == "Residuals") {
        statistics <- add_statistic(statistics, "df", x$Df[i], "df")
      } else {
        statistics <- statistics |>
          add_statistic("df numerator", x$Df[i], "df", "num.") |>
          add_statistic("df denominator", x$Df[[nrow(x)]], "df", "den.")
      }
    } else {
      statistics <- statistics |>
        add_statistic("df", x$Df[i]) |>
        add_statistic("residual df", x$`Resid. Df`[i], "df", "res.") |>
        add_statistic("residual df", x$Res.Df[i], "df", "res.") |>
        add_statistic("residual df", x$`Res. Df`[i], "df", "res.") |>
        add_statistic("df numerator", x$NumDF[i], "df", "num.") |>
        add_statistic("df denominator", x$DenDF[i], "df", "den.")
    }

    statistics <- statistics |>
      add_statistic("Rao", x$Rao[i]) |>
      add_statistic("p", x$`Pr(>F)`[i]) |>
      add_statistic("p", x$`Pr(>Chisq)`[i]) |>
      add_statistic("p", x$`Pr(>Chi)`[i]) |>
      add_statistic("Cp", x$Cp[i])

    group$statistics <- statistics
    groups$groups <- append(groups$groups, list(group))
  }

  analysis$groups <- append(analysis$groups, list(groups))

  analysis <- add_package_info(analysis, "stats")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'aov'
#' @keywords internal
tidy_stats.aov <- function(x, args = NULL) {
  # Create the analysis list and set the name and method
  analysis <- list(
    name = deparse(x$call[[2]]),
    method = "ANOVA"
  )

  # Get term statistics
  terms <- summary(x)[[1]]

  # Trim spaces from the names of the terms
  rownames(terms) <- stringr::str_trim(rownames(terms))

  # Create an empty groups list to add term statistics to
  groups <- list(name = "Terms")

  # Loop over the terms
  for (i in seq_len(nrow(terms))) {
    # Create a new group list
    group <- list(name = rownames(terms)[i])

    # Create a new statistics list and add the term's statistics
    statistics <- list()

    statistics <- statistics |>
      add_statistic("SS", terms$`Sum Sq`[i]) |>
      add_statistic("MS", terms$`Mean Sq`[i])

    # Special case: Extract different statistics depending on whether the term
    # is the Residuals term or not
    if (i != nrow(terms)) {
      statistics <- statistics |>
        add_statistic("statistic", terms$`F value`[i], "F") |>
        add_statistic("df numerator", terms$Df[i], "df", "num.") |>
        add_statistic(
          "df denominator",
          terms$Df[[nrow(terms)]],
          "df", "den."
        ) |>
        add_statistic("p", terms$`Pr(>F)`[i])
    } else {
      statistics <- add_statistic(statistics, "df", terms$Df[i])
    }

    # Add statistics to the group
    group$statistics <- statistics

    # Add the group to the groups list
    groups$groups <- append(groups$groups, list(group))
  }

  # Add the groups to the groups list on the analysis list
  analysis$groups <- append(analysis$groups, list(groups))

  # Add package information
  analysis <- add_package_info(analysis, "stats")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'aovlist'
#' @keywords internal
tidy_stats.aovlist <- function(x, args = NULL) {
  # Create the analysis list and set the name and method
  analysis <- list(
    name = deparse(attr(x, "call")),
    method = "ANOVA"
  )

  # Create a groups list to the the error strata in
  groups_error <- list(name = "Error terms")

  # Loop over the error strata
  for (i in seq_along(names(summary(x)))) {
    # Create a group for the error stratum
    group_error <- list(name = names(summary(x))[i])

    # Get term statistics of the current error stratum
    terms <- summary(x)[[i]][[1]]

    # Trim spaces from the names of the terms
    rownames(terms) <- stringr::str_trim(rownames(terms))

    # Create an empty groups list to add term statistics to
    groups <- list(name = "Terms")

    # Loop over the terms
    for (j in seq_len(nrow(terms))) {
      # Create a new group list
      group <- list(name = rownames(terms)[j])

      # Create a new statistics list and add the term's statistics
      statistics <- list()

      statistics <- add_statistic(statistics, "SS", terms$`Sum Sq`[j])
      statistics <- add_statistic(statistics, "MS", terms$`Mean Sq`[j])

      # Special case: Extract different statistics depending on whether the term
      # is the Residuals term or not
      if (j != nrow(terms)) {
        statistics <- add_statistic(
          statistics, "statistic", terms$`F value`[j],
          "F"
        )
        statistics <- add_statistic(
          statistics, "df numerator", terms$Df[j],
          "df", "num."
        )
        statistics <- add_statistic(
          statistics, "df denominator",
          terms$Df[[nrow(terms)]], "df", "den."
        )

        statistics <- add_statistic(statistics, "p", terms$`Pr(>F)`[j])
      } else {
        statistics <- add_statistic(statistics, "df", terms$Df[j])
      }

      # Add statistics to the group
      group$statistics <- statistics

      # Add the group to the groups list
      groups$groups <- append(groups$groups, list(group))
    }

    # Add the term group to the error groups
    group_error$groups <- append(group_error$groups, list(groups))

    # Add the error group to the error strata groups
    groups_error$groups <- append(groups_error$groups, list(group_error))
  }

  # Add the error stratum groups to the analysis
  analysis$groups <- append(analysis$groups, list(groups_error))

  # Add package information
  analysis <- add_package_info(analysis, "stats")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'confint'
#' @keywords internal
tidy_stats.confint <- function(x, args = NULL) {
  analysis <- list()

  analysis$method <- "Confidence intervals"

  groups <- list(name = "Coefficients")

  for (i in seq_along(rownames(x))) {
    group <- list(name = rownames(x)[i])

    group$statistics <- list() |>
      add_statistic("lower", x[i, 1]) |>
      add_statistic("upper", x[i, 2])

    groups$groups <- append(groups$groups, list(group))
  }

  analysis$groups <- append(analysis$groups, list(groups))

  bounds <- readr::parse_number(colnames(x))
  analysis$level <- diff(bounds) / 100

  analysis <- add_package_info(analysis, "stats")

  return(analysis)
}
