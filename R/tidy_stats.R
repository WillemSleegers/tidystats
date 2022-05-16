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
#' Currently supported functions are listed in the description of
#' \code{\link[=add_stats]{add_stats()}}
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
#' npk_aov <- aov(yield ~ block + N*P*K, npk)
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
#' @export
tidy_stats <- function(x, args = NULL) UseMethod("tidy_stats")

#' @describeIn tidy_stats tidy_stats method for class 'htest'
#' @export
tidy_stats.htest <- function(x, args = NULL) {
  # Create the analysis list and set the name
  if (str_detect(x$data.name, ",\n using scores:")) {
    x$data.name = paste0(gsub(" ,\n using scores:", " (scores:", x$data.name), ')')
  }
  analysis <- list(name = x$data.name)

  # Reduce Mauchly's test's method to one element
  if (x$method[1] == "Mauchly's test of sphericity") {
      x$method = "Mauchly's test of sphericity"
  }

  # Extract and clean up the method
  method <- dplyr::case_when(
    stringr::str_detect(x$method, "with simulated p-value") ~
      stringr::str_replace(x$method, "with simulated p-value(.|\n|\t)+",
        "(with simulated p-value)"),
    stringr::str_detect(x$method, "with continuity correction") ~
      stringr::str_replace(x$method, "with continuity correction",
        "(with continuity correction)"),
    stringr::str_detect(x$method, "without continuity correction") ~
      stringr::str_replace(x$method, "without continuity correction",
        "(without continuity correction)"),
    stringr::str_detect(x$method, "hybrid using asym") ~
      stringr::str_replace(x$method, "hybrid using asym(.)+", "(hybrid)"),
    stringr::str_detect(x$method, "Two Sample t-test") ~ trimws(x$method),
    stringr::str_detect(x$method, "One-way analysis of means") ~
      "One-way analysis of means",
    TRUE ~ x$method
  )

  # Add method to the analysis
  analysis$method <- method

  # Create a list to add the statistics to
  statistics <- list()

  # Extract statistics and add them to the statistics list, taking into account
  # several special cases
  # Special case: Calculate estimate for Two Sample t-tests
  if (!is.null(x$estimate)) {

    # Special case: Calculate the estimate as a mean difference in the case of
    # a two sample t-test
    # Special case: If there is more than 1 estimate, set the estimate to
    # NULL to skip it
    if (stringr::str_detect(method, "Two Sample t-test")) {
      value <- x$estimate[[1]] - x$estimate[[2]]
    } else if (length(x$estimate) > 1) {
      value <- NULL
    } else {
      value <- x$estimate[[1]]
    }

    # Set the name
    name <- dplyr::case_when(
      method == "One Sample t-test" ~ "mean",
      stringr::str_detect(method, "Two Sample t-test") |
        method == "Paired t-test"~ "mean difference",
      names(x$estimate)[1] == "ratio of variances" ~ "variance ratio",
      names(x$estimate)[1] == "probability of success" ~ "probability ratio",
      method == "One Sample t-test" ~ "mean",
      TRUE ~ names(x$estimate)
    )

    # Set the symbol
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
      names(x$estimate)[1] == "probability of success" ~ "P",
      names(x$estimate)[1] == "ratio of scales" ~ "ratio",
      names(x$estimate)[1] == "event rate" ~ "rate",
      names(x$estimate)[1] == "rate ratio" ~ "ratio",
      names(x$estimate)[1] == "common odds ratio" ~ "OR",
      stringr::str_detect(method, "t-test") ~ "M"
    )

    # Special case: In the case of a paired t-test and two sample t-test (Welch
    # or not), set the subscript to 'difference'
    subscript <- dplyr::case_when(
      stringr::str_detect(method, "Two Sample t-test") |
        method == "Paired t-test" ~ "diff.",
      names(x$estimate)[1] == "tau" ~ "τ",
      names(x$estimate)[1] == "rho" ~ "S",
      names(x$estimate)[1] == "difference in location" ~ "diff.",
      names(x$estimate)[1] == "event rate" ~ "event",
      names(x$estimate)[1] == "rate ratio" ~ "rate"
    )

    statistics <- add_statistic(statistics, "estimate", value, symbol,
      subscript, "CI", attr(x$conf.int, "conf.level"), x$conf.int[1],
      x$conf.int[2])
  }

  statistics <- add_statistic(statistics, "SE", x$stderr)

  # Set the symbol of the statistic
  if (!is.null(names(x$statistic))) {
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
      TRUE ~ names(x$statistic)
    )
    subscript <- dplyr::case_when(
      x$method == "Exact binomial test" ~ "successes",
      names(x$statistic) == "number of events" ~ "total",
      names(x$statistic) == "count1" ~ "event(1)"
    )
    name <-
      ifelse(symbol %in% c("k", "n"),
        "count",
        "statistic")
  }

  statistics = add_statistic(statistics, name, x$statistic[[1]], symbol, subscript)

  # Special case: One-way analysis of means has more than 1 df
  if (length(x$parameter) > 1) {
    statistics <-
      add_statistic(statistics, "df numerator", x$parameter[[1]],
                    "df", "num.")
    statistics <- add_statistic(statistics, "df denominator",
                                x$parameter[[2]], "df", "den.")
  } else if (x$method == "Phillips-Perron Unit Root Test") {
    statistics <-
      add_statistic(statistics, 'lag', as.numeric(x$parameter))
  } else if (x$method == "Exact binomial test") {
    statistics <- statistics %>%
      add_statistic("count", x$parameter[[1]], 'n', 'total') %>%
      add_statistic("statistic", x$null.value[[1]], 'P', 'expected')
  } else if (x$method == "Exact Poisson test") {
    statistics <-
      add_statistic(statistics, "statistic", x$parameter[[1]], 'T', 'time base')
  } else if (x$method == "Comparison of Poisson rates") {
    statistics <-
      add_statistic(statistics, "statistic", x$parameter[[1]], 'n', 'expected')
  } else{
    statistics <- add_statistic(statistics, "df", x$parameter[[1]])
  }

  statistics <- add_statistic(statistics, "p", x$p.value)

  # Add statistics to the analysis
  analysis$statistics <- statistics

  # Add additional information
  if (!is.null(x$alternative)) {
    alternative <- list(direction = x$alternative)

    if (!is.null(x$null.value)) {
      alternative$null_value <- x$null.value[[1]]
    }

    analysis$alternative <- alternative
  }

  if (stringr::str_detect(x$method, "simulated p-value")) {
    analysis$sim <- as.numeric(stringr::str_extract(x$method,
      "[0-9](e\\+)?([0-9].)?"))
  }

  if (stringr::str_detect(x$method, "hybrid")) {
    analysis$hybrid_parameters <- list(
      expect = readr::parse_number(
        stringr::str_extract(x$method, "exp=[0-9+]")
      ),
      percent = readr::parse_number(stringr::str_extract(x$method,
        "perc=[0-9]+")),
      Emin = readr::parse_number(stringr::str_extract(x$method, "exp=[0-9+]"))
    )
  }

  if (x$method == "Welch Two Sample t-test") {
    analysis$var_equal <- FALSE
  } else if (x$method == " Two Sample t-test") {
    analysis$var_equal <- TRUE
  } else if (x$method == "One-way analysis of means") {
    analysis$var_equal <- TRUE
  } else if (stringr::str_detect(x$method,
      "\\(not assuming equal variances\\)")) {
    analysis$var_equal <- FALSE
  }

  # Add package information
  analysis <- add_package_info(analysis, "stats")

  return(analysis)
}


#' @describeIn tidy_stats tidy_stats method for class 'pairwise.htest'
#' @export
tidy_stats.pairwise.htest <- function(x, args = NULL) {
  # Create the analysis list and set the name
  analysis <- list(name = x$data.name)

  # Add method to the analysis
  analysis$method <-
    ifelse(startsWith(x$method, "Pairwise"),
      x$method,
      paste("Pairwise", x$method))

  # Check if there is 1 or more terms
  # If 1, only create a statistics list
  # If multiple, loop over terms and create separate lists for each term
  if (nrow(x$p.value) == 1) {
    statistics <- list()
    statistics <- add_statistic(statistics, "p", x$p.value[1])
    analysis$statistics <- statistics
  } else {
    groups <- list(name = "P-values")

    for (i in 1:nrow(x$p.value)) {
      group <- list(name = rownames(x$p.value)[i])

      statistics <- list()
      for (j in 1:ncol(x$p.value)) {
        statistics <-
          add_statistic(statistics, "p",
          x$p.value[i, j], subscript = colnames(x$p.value)[j])
      }

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
#' @export
tidy_stats.lm <- function(x, args = NULL) {
  # Create the analysis list and set the name and method
  analysis <- list(
    name = deparse(x$call[[2]]),
    method = "Linear regression"
  )

  # Get summary statistics
  summary <- summary(x)

  # Model fit
  # Create a group and statistics list for the model fit statistics
  group <- list(name = "Model")
  statistics <- list()

  # Extract and add statistics to the statistics list
  statistics <- add_statistic(statistics, "R squared", summary$r.squared, "R²")
  statistics <- add_statistic(statistics, "adj. R squared",
    summary$adj.r.squared, "R²", "adj.")
  statistics <- add_statistic(statistics, "statistic", summary$fstatistic[[1]],
    "F")
  statistics <- add_statistic(statistics, "df numerator",
    summary$fstatistic[[2]], "df", "num.")
  statistics <- add_statistic(statistics, "df denominator",
    summary$fstatistic[[3]], "df", "den.")
  statistics <- add_statistic(statistics, "p",
    stats::pf(summary$fstatistic[[1]], summary$fstatistic[[2]],
      summary$fstatistic[[3]], lower.tail = FALSE))
  statistics <- add_statistic(statistics, "sigma", summary$sigma, "s",
    "res.")

  # Add statistics to the group
  group$statistics <- statistics

  # Add the model group to a groups element on the analysis
  analysis$groups <- append(analysis$groups, list(group))

  # Create a groups list for the coefficients
  groups <- list(name = "Coefficients")

  # Extract statistics of the coefficients
  coefs <- stats::coef(summary)

  # Loop over the coefficients and add statistics to a group list
  for (i in 1:nrow(coefs)) {
    # Create a new group list
    group <- list()

    # Add the name and type of the coefficient
    group$name <- rownames(coefs)[i]

    # Create a new statistics list
    statistics <- list()

    statistics <- add_statistic(statistics, "estimate", coefs[i, "Estimate"],
      "b")
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std. Error"])
    statistics <- add_statistic(statistics, "statistic", coefs[i, "t value"],
      "t")
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
#' @export
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
  statistics <- add_statistic(statistics, "null deviance",
    summary$null.deviance, "D", "null")
  statistics <- add_statistic(statistics, "residual deviance", summary$deviance,
    "D", "res.")
  statistics <- add_statistic(statistics, "null df", summary$df.null, "df",
    "null")
  statistics <- add_statistic(statistics, "residual df", summary$df.residual,
    "df", "res.")
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
  for (i in 1:nrow(coefs)) {
    # Create a new group list
    group <- list()

    # Add the name and type of the coefficient
    group$name <- rownames(coefs)[i]

    # Create a new statistics list
    statistics <- list()

    statistics <- add_statistic(statistics, "estimate", coefs[i, "Estimate"],
      "b")
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std. Error"])
    statistics <- add_statistic(statistics, "statistic", coefs[i, 3],
      dplyr::if_else(colnames(coefs)[3] == "z value", "z", "t"))
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
#' @export
tidy_stats.anova <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()

  # Extract the heading
  heading <- attr(x, "heading")

  # Determine the method
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
  if (sum(stringr::str_detect(heading, "Models:")) > 0) {
    model_comparison = TRUE
  } else {
    model_comparison = FALSE
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
        heading[2], "\n")
      ), "Model [0-9+]: "
    )
  }

  # Replace NULL with (Intercept) in the case of a single model ANODE
  if (!model_comparison & method == "ANODE") {
    if (rownames(x)[1] == "NULL") {
      rownames(x)[1] <- "(Intercept)"
    }
  }

  # Create an empty groups list to add model or term statistics to
  groups <- list(name = dplyr::if_else(model_comparison, "Models", "Terms"))

  # Loop over each row
  for (i in 1:nrow(x)) {
    # Create a new group list
    group <- list()

    # Set the name
    if (!is.null(rownames(x)[i])) {
      group$name <- rownames(x)[i]
    } else if (!is.null(x$name[i])) {
      group$name <- x$name[i] #TODO: Check if this one is necessary
    }

    # Create a new statistics list and add statistics
    statistics <- list()

    statistics <- add_statistic(statistics, "n parameters", x$npar[i], "k")
    statistics <- add_statistic(statistics, "AIC", x$AIC[i])
    statistics <- add_statistic(statistics, "BIC", x$BIC[i])
    statistics <- add_statistic(statistics, "log likelihood", x$logLik[i], "l")
    statistics <- add_statistic(statistics, "deviance", x$deviance[i], "D")
    statistics <- add_statistic(statistics, "deviance", x$Deviance[i], "D")
    statistics <- add_statistic(statistics, "residual deviance",
      x$`Resid. Dev`[i], "D", "res.")
    statistics <- add_statistic(statistics, "RSS", x$RSS[i])
    statistics <- add_statistic(statistics, "SS", x$`Sum Sq`[i])
    statistics <- add_statistic(statistics, "SS", x$`Sum of Sq`[i])
    statistics <- add_statistic(statistics, "MS", x$`Mean Sq`[i])
    statistics <- add_statistic(statistics, "statistic", x$Chisq[i],
      "χ²")

    if ("F" %in% names(x)) {
      statistics <- add_statistic(statistics, "statistic", x$`F`[i], "F")
    } else {
      statistics <- add_statistic(statistics, "statistic", x$`F value`[i], "F")
    }

    # Special case: Degrees of freedom
    if (method == "ANOVA" & !model_comparison) {
      if (rownames(x)[i] != "Residuals") {
        statistics <- add_statistic(statistics, "df numerator", x$Df[i], "df",
          "num.")
        statistics <- add_statistic(statistics, "df denominator",
          x$Df[[nrow(x)]], "df", "den.")
      } else {
        statistics <- add_statistic(statistics, "df", x$Df[i])
      }
    } else {
      statistics <- add_statistic(statistics, "df", x$Df[i])
      statistics <- add_statistic(statistics, "residual df", x$Res.Df[i], "df",
        "res.")
      statistics <- add_statistic(statistics, "residual df", x$`Res. Df`[i],
        "df", "res.")
      statistics <- add_statistic(statistics, "df numerator", x$NumDF[i], "df",
        "num.")
      statistics <- add_statistic(statistics, "df denominator", x$DenDF[i],
        "df", "den.")
    }

    statistics <- add_statistic(statistics, "Rao", x$Rao[i])
    statistics <- add_statistic(statistics, "p", x$`Pr(>F)`[i])
    statistics <- add_statistic(statistics, "p", x$`Pr(>Chisq)`[i])
    statistics <- add_statistic(statistics, "Cp", x$Cp[i])

    # Add statistics to the group
    group$statistics <- statistics

    # Add the group to the groups list
    groups$groups <- append(groups$groups, list(group))
  }

  # Add the groups to the groups list on the analysis list
  analysis$groups <- append(analysis$groups, list(groups))

  # Add additional information
  if (method == "ANODE" & model_comparison) {
    analysis$family <- stringr::str_extract(heading, "(?<=Model: ).*(?=,)")
    analysis$link <- stringr::str_extract(heading, "(?<=link: ).*")
  }

  # Add package information
  analysis <- add_package_info(analysis, "stats")

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
    "Terms"))

  # Loop over the models or terms
  for (i in 1:length(rownames(x))) {
    # Create a group list and set the name
    group <- list(name = rownames(x)[i])

    # Create a statistics list and add statistics
    statistics <- list()

    statistics <- add_statistic(statistics, "AIC", x$AIC[i])
    statistics <- add_statistic(statistics, "BIC", x$BIC[i])
    statistics <- add_statistic(statistics, "log likelihood", x$logLik[i], "l")
    statistics <- add_statistic(statistics, "likelihood ratio", x$L.Ratio[i],
      "LR")
    statistics <- add_statistic(statistics, "df", x$df[i])
    statistics <- add_statistic(statistics, "df numerator", x$NumDF[i], "df",
      "num.")
    statistics <- add_statistic(statistics, "df denominator", x$DenDF[i], "df",
      "den.")
    statistics <- add_statistic(statistics, "statistic", x$`F-value`[i], "F")
    statistics <- add_statistic(statistics, "p", x$p-value[i])

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

#' @describeIn tidy_stats tidy_stats method for class 'aov'
#' @export
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
  for (i in 1:nrow(terms)) {
    # Create a new group list
    group <- list(name = rownames(terms)[i])

    # Create a new statistics list and add the term's statistics
    statistics <- list()

    statistics <- add_statistic(statistics, "SS", terms$`Sum Sq`[i])
    statistics <- add_statistic(statistics, "MS", terms$`Mean Sq`[i])

    # Special case: Extract different statistics depending on whether the term
    # is the Residuals term or not
    if (i != nrow(terms)) {
      statistics <- add_statistic(statistics, "statistic", terms$`F value`[i],
        "F")
      statistics <- add_statistic(statistics, "df numerator", terms$Df[i], "df",
        "num.")
      statistics <- add_statistic(statistics, "df denominator",
        terms$Df[[nrow(terms)]], "df", "den.")

      statistics <- add_statistic(statistics, "p", terms$`Pr(>F)`[i])
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
#' @export
tidy_stats.aovlist <- function(x, args = NULL) {
  # Create the analysis list and set the name and method
  analysis <- list(
    name = deparse(attr(x, "call")),
    method = "ANOVA"
  )

  # Create a groups list to the the error strata in
  groups_error <- list(name = "Error terms")

  # Loop over the error strata
  for (i in 1:length(names(summary(x)))) {
    # Create a group for the error stratum
    group_error <- list(name = names(summary(x))[i])

    # Get term statistics of the current error stratum
    terms <- summary(x)[[i]][[1]]

    # Trim spaces from the names of the terms
    rownames(terms) <- stringr::str_trim(rownames(terms))

    # Create an empty groups list to add term statistics to
    groups <- list(name = "Terms")

    # Loop over the terms
    for (j in 1:nrow(terms)) {
      # Create a new group list
      group <- list(name = rownames(terms)[j])

      # Create a new statistics list and add the term's statistics
      statistics <- list()

      statistics <- add_statistic(statistics, "SS", terms$`Sum Sq`[j])
      statistics <- add_statistic(statistics, "MS", terms$`Mean Sq`[j])

      # Special case: Extract different statistics depending on whether the term
      # is the Residuals term or not
      if (j != nrow(terms)) {
        statistics <- add_statistic(statistics, "statistic", terms$`F value`[j],
          "F")
        statistics <- add_statistic(statistics, "df numerator", terms$Df[j],
          "df", "num.")
        statistics <- add_statistic(statistics, "df denominator",
          terms$Df[[nrow(terms)]], "df", "den.")

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

#' @describeIn tidy_stats tidy_stats method for class 'tidystats_descriptives'
#' @export
tidy_stats.tidystats_descriptives <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()

  # Extract variable information
  var_names <- unique(dplyr::pull(x, var))

  # Set the name if there is only 1 variable
  if (length(var_names) == 1) {
    analysis$name <- var_names
  }

  # Set method
  analysis$method <- "Descriptives"

  # Create a loop function to recursively create groups and extract the
  # statistics
  loop <- function(df, list, group_names, depth) {
    if (length(group_names) == depth) {
      # Create a list to store the statistics in
      statistics <- list()

      # Add statistics
      statistics <- add_statistic(statistics, "missing", df$missing)
      statistics <- add_statistic(statistics, "N", df$N)
      statistics <- add_statistic(statistics, "mean", df$M, "M")
      statistics <- add_statistic(statistics, "standard deviation", df$SD, "SD")
      statistics <- add_statistic(statistics, "standard error", df$SE, "SE")
      statistics <- add_statistic(statistics, "minimum", df$min, "min")
      statistics <- add_statistic(statistics, "maximum", df$maxm, "max")
      statistics <- add_statistic(statistics, "range", df$range)
      statistics <- add_statistic(statistics, "median", df$median, "Mdn")
      statistics <- add_statistic(statistics, "mode", df$mode)
      statistics <- add_statistic(statistics, "skew", df$skew)
      statistics <- add_statistic(statistics, "kurtosis", df$kurtosis)

      # Add statistics to the group
      list$statistics <- statistics
    } else {
      # Increment the depth
      depth <- depth + 1

      # Create a groups list
      groups <- list(name = group_names[depth])

      # Loop over the groups
      for (group_name in unique(pull(df, groups$name))) {
        # Subset the data so it only has data of the current group
        df_group <- df[df[, depth + 1] == group_name, ]

        if (!is.na(group_name)) {
          df_group <- dplyr::filter(df,
            dplyr::if_all(dplyr::all_of(groups$name), ~ . == group_name)
          )
        } else {
          df_group <- dplyr::filter(df,
            dplyr::if_all(dplyr::all_of(groups$name), is.na)
          )
        }

        # Create a group list
        group <- list(name = group_name)

        # Loop again
        groups$groups <- append(groups$groups, list(loop(df_group, group,
          group_names, depth)))
      }

      # Add the groups to the list's groups
      list$groups <- append(list$groups, list(groups))
    }

    return(list)
  }

  # Extract grouping information
  group_names <- dplyr::group_vars(x)

  # Convert the data frame to a base data frame to disable warnings
  df <- as.data.frame(x)

  # Get the groups and statistics and loop over the variables if there are
  # more than one
  if (length(var_names) == 1) {
    analysis <- loop(df, analysis, group_names, 0)
  } else {
    for (var_name in var_names) {
      # Filter the data frame to have only the rows belonging to this variable
      df_var <- dplyr::filter(df, var == var_name)

      # Create a list for the variable
      group <- list(name = var_name)

      # Loop
      group <- loop(df_var, group, group_names, 0)

      # Add the lists to the groups element of the analysis
      analysis$groups <- append(analysis$groups, list(group))
    }
  }

  # Add package information
  analysis <- add_package_info(analysis, "tidystats")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'tidystats_counts'
#' @export
tidy_stats.tidystats_counts <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()

  # Add method
  analysis$method <- "Counts"

  # Create a loop function to recursively create groups and extract the
  # statistics
  loop <- function(df, list, group_names, depth) {
    if (length(group_names) == depth) {
      # Create a list to store the statistics in
      statistics <- list()

      # Add statistics
      statistics <- add_statistic(statistics, "n", df$n)
      statistics <- add_statistic(statistics, "pct", df$pct, "%")

      # Add statistics to the group
      list$statistics <- statistics
    } else {
      # Increment the depth
      depth <- depth + 1

      # Create a groups list
      groups <- list(name = group_names[depth])

      # Loop over the groups
      for (group_name in unique(dplyr::pull(df, groups$name))) {
        # Subset the data so it only has data of the current group
        df_group <- df[df[, depth + 1] == group_name, ]

        if (!is.na(group_name)) {
          df_group <- dplyr::filter(df,
            dplyr::if_all(groups$name, ~ . == group_name)
          )
        } else {
          df_group <- dplyr::filter(df,
            dplyr::if_all(groups$name, is.na)
          )
        }

        # Create a group list
        group <- list()

        # Set the name to the string NA if it is missing
        if (is.na(group_name)) {
          group$name <- "NA"
        } else {
          group$name <- group_name
        }

        # Loop again
        groups$groups <- append(groups$groups, list(loop(df_group, group,
          group_names, depth)))
      }

      # Add the groups to the list's groups
      list$groups <- append(list$groups, list(groups))
    }

    return(list)
  }

  # Extract grouping variables
  group_names <- names(x)[!names(x) %in% c("n", "pct")]

  # Convert the data frame to a base data frame to disable warnings
  df <- as.data.frame(x)

  # Loop over the groups and extract the statistics
  analysis <- loop(df, analysis, group_names, 0)

  # Add package information
  analysis <- add_package_info(analysis, "tidystats")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'lmerMod'
#' @export
tidy_stats.lmerMod <- function(x, args = NULL) {
  # Create a list to store the analysis in and set the name and method
  analysis <- list(
    name = deparse(attr(x@frame, "formula")),
    method = "Linear mixed model"
  )

  # Get summary statistics
  summary <- summary(x)

  # Model fit
  # Create a group and statistics list for the model fit statistics
  group_model <- list(name = "Model")
  statistics <- list()

  # Check if the AIC is in the AICtab, if so, add the statistics
  if ("AIC" %in% names(summary$AICtab)) {
    statistics <- add_statistic(statistics, "AIC", summary$AICtab[["AIC"]])
    statistics <- add_statistic(statistics, "BIC", summary$AICtab[["BIC"]])
    statistics <- add_statistic(statistics, "log likelihood",
      summary$AICtab[["logLik"]], "l")
    statistics <- add_statistic(statistics, "deviance",
      summary$AICtab[["deviance"]], "D")
    statistics <- add_statistic(statistics, "residual df",
      summary$AICtab[["df.resid"]], "df", "res.")
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
  for (i in 1:length(varcor)) {
    # Create lists for the group
    group_RE_group <- list(name = names(varcor)[i])
    group_RE_variances <- list(name = "Variances")

    # Set N for the group, if there is an N
    if (names(varcor)[i] %in% names(summary$ngrps)) {
      statistics <- list()
      statistics <- add_statistic(statistics, "N",
        summary$ngrps[names(summary$ngrps) == names(varcor)[i]][[1]])

      group_RE_group$statistics <- statistics
    }

    # Extract standard deviations
    SDs <- attr(varcor[[i]], "stddev")

    # Loop over variances in the group
    for (j in 1:length(SDs)) {
      group_RE_group_variance <- list(name = names(SDs)[j])

      # Extract statistics
      statistics <- list()

      statistics <- add_statistic(statistics, "standard deviation", SDs[[j]],
        "SD")
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
      for (j in 1:nrow(cors)) {
        group_RE_correlation <- list(
          name = paste(cors$name1[j], "-", cors$name2[j])
        )

        statistics <- list()
        statistics <- add_statistic(statistics, "correlation", cors$value[j],
          "r")

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
  statistics <- add_statistic(statistics, "standard deviation",
    attr(varcor, "sc"), "SD")
  statistics <- add_statistic(statistics, "variance", attr(varcor, "sc")^2,
    "var")

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
  for (i in 1:nrow(coefs)) {
    # Create a list for the coefficient and set the name
    group_FE_coefficient <- list(name = rownames(coefs)[i])

    # Create a new statistics list and add the fixed effect's statistics
    statistics <- list()

    statistics <- add_statistic(statistics, "estimate", coefs[i, "Estimate"],
      "b")
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std. Error"])
    statistics <- add_statistic(statistics, "statistic", coefs[i, "t value"],
      "t")

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
    for (i in 1:nrow(fixed_cors)) {
      group_FE_pair <- list(
        name = paste(fixed_cors$name1[i], "-", fixed_cors$name2[i])
      )

      statistics <- list()
      statistics <- add_statistic(statistics, "correlation",
        fixed_cors$value[i], "r")

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
  analysis$convergence_code = summary$optinfo$conv$opt
  analysis$convergence_message = summary$optinfo$conv$lme4$messages

  # Add package information
  analysis <- add_package_info(analysis, "lme4")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'lmerModLmerTest'
#' @export
tidy_stats.lmerModLmerTest <- function(x, args = NULL) {
  # Create a list to store the analysis in and set the name and method
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

  # Get summary statistics
  summary <- summary(x)

  # Model fit
  # Create a group and statistics list for the model fit statistics
  group_model <- list(name = "Model")
  statistics <- list()

  # Check if the AIC is in the AICtab, if so, add the statistics
  if ("AIC" %in% names(summary$AICtab)) {
    statistics <- add_statistic(statistics, "AIC", summary$AICtab[["AIC"]])
    statistics <- add_statistic(statistics, "BIC", summary$AICtab[["BIC"]])
    statistics <- add_statistic(statistics, "log likelihood",
      summary$AICtab[["logLik"]], "l")
    statistics <- add_statistic(statistics, "deviance",
      summary$AICtab[["deviance"]], "D")
    statistics <- add_statistic(statistics, "residual df",
      summary$AICtab[["df.resid"]], "df", "res.")
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
  for (i in 1:length(varcor)) {
    # Create lists for the group
    group_RE_group <- list(name = names(varcor)[i])
    group_RE_variances <- list(name = "Variances")

    # Set N for the group, if there is an N
    if (names(varcor)[i] %in% names(summary$ngrps)) {
      statistics <- list()
      statistics <- add_statistic(statistics, "N",
        summary$ngrps[names(summary$ngrps) == names(varcor)[i]][[1]])

      group_RE_group$statistics <- statistics
    }

    # Extract standard deviations
    SDs <- attr(varcor[[i]], "stddev")

    # Loop over variances in the group
    for (j in 1:length(SDs)) {
      group_RE_group_variance <- list(name = names(SDs)[j])

      # Extract statistics
      statistics <- list()

      statistics <- add_statistic(statistics, "standard deviation", SDs[[j]],
        "SD")
      statistics <- add_statistic(statistics, "variance", SDs[[j]]^2, "var")

      # Add statistics to the RE group
      group_RE_group_variance$statistics <- statistics

      # Add RE group coefficients to the RE group
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
      for (j in 1:nrow(cors)) {
        group_RE_correlation <- list(
          name = paste(cors$name1[j], "-", cors$name2[j])
        )

        statistics <- list()
        statistics <- add_statistic(statistics, "correlation", cors$value[j],
          "r")

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
  statistics <- add_statistic(statistics, "standard deviation",
    attr(varcor, "sc"), "SD")
  statistics <- add_statistic(statistics, "variance", attr(varcor, "sc")^2,
    "var")

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
  for (i in 1:nrow(coefs)) {
    # Create a list for the coefficient and set the name
    group_FE_coefficient <- list(name = rownames(coefs)[i])

    # Create a new statistics list and add the fixed effect's statistics
    statistics <- list()

    statistics <- add_statistic(statistics, "estimate", coefs[i, "Estimate"],
      "b")
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std. Error"])
    statistics <- add_statistic(statistics, "df", coefs[i, "df"])
    statistics <- add_statistic(statistics, "statistic", coefs[i, "t value"],
      "t")
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
    for (i in 1:nrow(fixed_cors)) {
      group_FE_pair <- list(
        name = paste(fixed_cors$name1[i], "-", fixed_cors$name2[i])
      )

      statistics <- list()
      statistics <- add_statistic(statistics, "correlation",
        fixed_cors$value[i], "r")

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
  analysis$convergence_code = summary$optinfo$conv$opt
  analysis$convergence_message = summary$optinfo$conv$lme4$messages

  # Add package information
  analysis <- add_package_info(analysis, "lmerTest")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'nlme'
#' @export
tidy_stats.lme <- function(x, args = NULL) {
  # Create a list to store the analysis in and set the method
  analysis <- list(
    method = "Linear mixed-effects model"
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
      coefs <- bind_rows(coefs, tibble(group = "Residual",
        var = varcor[i, "Variance"], sd = varcor[i, "StdDev"]))
    } else {
      coefs <- bind_rows(coefs, tibble(group = group, coef = rowname,
        var = varcor[i, "Variance"], sd = varcor[i, "StdDev"]))

      if ("Corr" %in% colnames(varcor)) {
        if (varcor[i, "Corr"] != "") {
          value <- suppressWarnings(as.numeric(varcor[i, "Corr"]))
          if (is.na(value)) {

            rownames(varcor)[]

            colnames <- rownames(varcor)[(group_row + 1):(group_row +
                length(colnames(varcor))-2)]
          } else {
            c(rownames(varcor)[i], varcor[i, 3:length(colnames(varcor))])

            corrs <- suppressMessages(rbind(corrs,
              c(group, rownames(varcor)[i],
                varcor[i, 3:length(colnames(varcor))])))
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

      statistics <- add_statistic(statistics, "N",
        x$dims$ngrps[stringr::str_detect(group_name, names(x$dims$ngrps))][[1]])

      group_RE_group$statistics <- statistics

      # Subset the coefs data from to only the current group
      group_coefs <- dplyr::filter(coefs, group == group_name)

      for (i in 1:nrow(group_coefs)) {
        group_RE_group_coefficient <- list(name = group_coefs$coef[i])

        # Extract statistics
        statistics <- list()

        statistics <- add_statistic(statistics, "standard deviation",
          group_coefs$sd[i], "SD")
        statistics <- add_statistic(statistics, "variance", group_coefs$var[i],
          "var")

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

      statistics <- add_statistic(statistics, "standard deviation",
        as.numeric(varcor[i, "StdDev"]), "SD")
      statistics <- add_statistic(statistics, "variance",
        as.numeric(varcor[i, "Variance"]), "var")

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
        group_RE_pair <- list(name = paste(pairs$name1[i], "-", pairs$name2[i]))

        #
        statistics <- list()
        statistics <- add_statistic(statistics, "correlation", pairs$value[i],
          "r")

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

    statistics <- add_statistic(statistics, "estimate", coefs[i, "Value"],
      "b")
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std.Error"])
    statistics <- add_statistic(statistics, "df", coefs[i, "DF"])
    statistics <- add_statistic(statistics, "statistic", coefs[i, "t-value"],
      "t")
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
      group_FE_pair <- list(
        name = paste(fixed_cors$name1[i], "-", fixed_cors$name2[i])
      )

      statistics <- list()
      statistics <- add_statistic(statistics, "correlation",
        fixed_cors$value[i], "r")

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
      coefs <- bind_rows(coefs, tibble(group = "Residual",
        var = varcor[i, "Variance"], sd = varcor[i, "StdDev"]))
    } else {
      coefs <- bind_rows(coefs, tibble(group = group, coef = rowname,
        var = varcor[i, "Variance"], sd = varcor[i, "StdDev"]))

      if ("Corr" %in% colnames(varcor)) {
        if (varcor[i, "Corr"] != "") {
          value <- suppressWarnings(as.numeric(varcor[i, "Corr"]))
          if (is.na(value)) {

            rownames(varcor)[]

            colnames <- rownames(varcor)[(group_row + 1):(group_row +
                length(colnames(varcor))-2)]
          } else {
            c(rownames(varcor)[i], varcor[i, 3:length(colnames(varcor))])

            corrs <- suppressMessages(rbind(corrs,
              c(group, rownames(varcor)[i],
                varcor[i, 3:length(colnames(varcor))])))
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

      statistics <- add_statistic(statistics, "N",
        x$dims$ngrps[stringr::str_detect(group_name, names(x$dims$ngrps))][[1]])

      group_RE_group$statistics <- statistics

      # Subset the coefs data from to only the current group
      group_coefs <- dplyr::filter(coefs, group == group_name)

      for (i in 1:nrow(group_coefs)) {
        group_RE_group_coefficient <- list(name = group_coefs$coef[i])

        # Extract statistics
        statistics <- list()

        statistics <- add_statistic(statistics, "standard deviation",
          group_coefs$sd[i], "SD")
        statistics <- add_statistic(statistics, "variance", group_coefs$var[i],
          "var")

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

      statistics <- add_statistic(statistics, "standard deviation",
        as.numeric(varcor[i, "StdDev"]), "SD")
      statistics <- add_statistic(statistics, "variance",
        as.numeric(varcor[i, "Variance"]), "var")

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
        group_RE_pair <- list(name = paste(pairs$name1[i], "-", pairs$name2[i]))

        #
        statistics <- list()
        statistics <- add_statistic(statistics, "correlation", pairs$value[i],
          "r")

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

    statistics <- add_statistic(statistics, "estimate", coefs[i, "Value"],
      "b")
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std.Error"])
    statistics <- add_statistic(statistics, "df", coefs[i, "DF"])
    statistics <- add_statistic(statistics, "statistic", coefs[i, "t-value"],
      "t")
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
      group_FE_pair <- list(
        name = paste(fixed_cors$name1[i], "-", fixed_cors$name2[i])
      )

      statistics <- list()
      statistics <- add_statistic(statistics, "correlation",
        fixed_cors$value[i], "r")

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

    statistics <- add_statistic(statistics, "estimate", coefs[i, "Value"],
      "b")
    statistics <- add_statistic(statistics, "SE", coefs[i, "Std.Error"])
    statistics <- add_statistic(statistics, "statistic", coefs[i, "t-value"],
      "t")
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
      group_correlation <- list(
        name = paste(cors$name1[i], "-", cors$name2[i])
      )

      statistics <- list()
      statistics <- add_statistic(statistics, "correlation",
        cors$value[i], "r")

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

#' @describeIn tidy_stats tidy_stats method for class 'BayesFactor'
#' @export
tidy_stats.BFBayesFactor <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()

  # Determine and set the method
  class <- class(x@numerator[[1]])[1]
  analysis$method <- dplyr::case_when(
    class == "BFoneSample" ~ "Bayesian t-test",
    class == "BFlinearModel" ~ "Bayesian linear regression",
    class == "BFcorrelation" ~ "Bayesian correlation",
    class == "BFcontingencyTable" ~ "Bayesian contingency table",
    class == "BFproportion" ~ "Bayesian analysis of proportions",
    class == "BFmetat" ~ "Bayesian meta-analysis"
  )

  # Extract bayes factors
  bayes_factors <- BayesFactor::extractBF(x)

  # Extract the statistics or loop over the models
  if (nrow(bayes_factors) == 1) {
    # Create a statistics list
    statistics <- list()

    # Extract statistics
    statistics <- add_statistic(statistics, "BF10", bayes_factors$bf, "BF",
      "10")
    statistics <- add_statistic(statistics, "BF01", 1/bayes_factors$bf, "BF",
      "01")
    statistics <- add_statistic(statistics, "proportional error",
      bayes_factors$error, "PE")

    # Add statistics to the analysis
    analysis$statistics <- statistics
  } else {
    # Create a list to store the different models in
    groups <- list(name = "Models")

    # Loop over the models
    for (i in 1:nrow(bayes_factors)) {
      # Create a list to store the model statistics in
      group <- list(name = rownames(bayes_factors)[i])

      # Create a list to add the statistics to
      statistics <- list()

      statistics <- add_statistic(statistics, "BF10", bayes_factors$bf[i], "BF",
        "10")
      statistics <- add_statistic(statistics, "BF01", 1/bayes_factors$bf[i],
        "BF", "01")
      statistics <- add_statistic(statistics, "proportional error",
        bayes_factors$error[i], "PE")

      # Add statistics to the model list
      group$statistics <- statistics

      # Add the model group to the groups list
      groups$groups <- append(groups$groups, list(group))
    }

    # Add the list of models to the analysis list
    analysis$groups <- append(analysis$groups, list(groups))
  }

  # Add denominator model information
  alternative <- list(
    name = x@denominator@longName,
    formula = x@denominator@identifier$formula
  )
  analysis$alternative <- alternative

  # Add package information
  analysis <- add_package_info(analysis, "BayesFactor")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'afex_aov'
#' @export
tidy_stats.afex_aov <- function(x, args = NULL) {
  # Create the analysis list and set the name and method
  analysis <- list(name = attr(x, "dv"), method = "ANOVA")

  # Get term statistics
  terms <- x$anova_table

  # Create an empty groups list to add term statistics to
  groups <- list(name = "Terms")

  # Loop over the terms
  for (i in 1:nrow(terms)) {
    # Create a new group list
    group <- list(name = rownames(terms)[i])

    # Create a new statistics list and add the term's statistics
    statistics <- list()

    statistics <- add_statistic(statistics, "df numerator", terms$`num Df`[i],
      "df", "num.")
    statistics <- add_statistic(statistics, "df denominator", terms$`den Df`[i],
      "df", "den.")
    statistics <- add_statistic(statistics, "MSE", terms$MSE[i])
    statistics <- add_statistic(statistics, "statistic", terms$`F`[i], "F")
    statistics <- add_statistic(statistics, "ges", terms$ges[i], "η²", "G")
    statistics <- add_statistic(statistics, "pes", terms$pes[i], "η²", "p")
    statistics <- add_statistic(statistics, "p", terms$`Pr(>F)`[i])

    # Add statistics to the group
    group$statistics <- statistics

    # Add the group to the groups list
    groups$groups <- append(groups$groups, list(group))
  }

  # Add the groups to the groups list on the analysis list
  analysis$groups <- append(analysis$groups, list(groups))

  # Add additional information
  analysis$anova_type <- attr(x, "type")
  analysis$p_adjustment_method <- attr(x$anova_table, "p_adjust_method")
  analysis$sphericity_correction_method <- attr(x$anova_table, "correction")

  # Add package information
  analysis <- add_package_info(analysis, "afex")

  return(analysis)
}


#' @describeIn tidy_stats tidy_stats method for class 'emmGrid'
#' @export
tidy_stats.emmGrid <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()

  # Determine and set method
  type <- x@misc$estType

  if (type == "contrast" | type == "pairs") {
    analysis$method <- "contrasts"
  } else {
    analysis$method <- "EMMs"
  }

  # Convert object to a data frame
  df <- as.data.frame(x)


  # Determine type of analysis
  by <- x@misc$by.vars

  x@misc$by.vars
  x@misc$pri.vars

  if (!is.null(by)) {

    # Create an empty groups list
    groups <- list()

    for (i in 1:length(levels(df[, by]))) {
      # Create an empty group
      group <- list()

      # Set the group name
      name <- levels(df[, by])[i]
      group$name <- paste(by, "=", name)

      # Create an empty terms list
      terms <- list()

      # Filter the statistics of this group
      df_group <- df[df[by] == name, ]

      for (j in 1:nrow(df_group)) {

        # Create a new term list
        term <- list()

        # Add the name of the term
        if (type == "contrast" | type == "pairs") {
          term$name <- as.character(df_group$contrast[j])
        } else if (type == "prediction") {
          term$name <- paste(
            x@misc$pri.vars, "=", df_group[, x@misc$pri.vars][j]
          )
        } else {
          term$name <- df_group$terms[j]
        }

        # Create a new statistics list and add the term's statistics
        statistics <- list()

        if (type == "contrast" | type == "pairs") {
          statistics$estimate$name <- "mean difference"
          statistics$estimate$value <- df_group$estimate[j]
          statistics$SE <- df_group$SE[j]
          statistics$df <- df_group$df[j]
          statistics$statistic$name <- "t"
          statistics$statistic$value <- df_group$t.ratio[j]
          statistics$p <- df_group$p.value[j]
        } else {
          statistics$EMM <- df_group$emmean[j]
          statistics$SE <- df_group$SE[j]
          statistics$df <- df_group$df[j]
          statistics$CI$CI_level <- x@misc$level
          statistics$CI$CI_lower <- df_group$lower.CL[j]
          statistics$CI$CI_upper <- df_group$upper.CL[j]
        }

        term$statistics <- statistics

        # Add the term data to the coefficients list
        terms[[j]] <- term
      }

      # Add coefficients to the group
      group$terms <- terms

      # Add group to the groups list
      groups[[i]] <- group
    }

    # Add groups to the output
    output$groups <- groups
  } else {
    # Create an empty terms list
    terms <- list()

    for (i in 1:nrow(df)) {

      # Create a new term list
      term <- list()

      # Add the name of the term
      term$name <- df$contrast[i]

      # Create a new statistics list and add the term's statistics
      statistics <- list()

      statistics$estimate$name <- "b"
      statistics$estimate$value <- df$estimate[i]
      statistics$SE <- df$SE[i]
      statistics$df <- df$df[i]
      statistics$statistic$name <- "t"
      statistics$statistic$value <- df$t.ratio[i]
      statistics$p <- df$p.value[i]

      term$statistics <- statistics

      # Add the term data to the coefficients list
      terms[[i]] <- term
    }

    # Add terms to the output
    output$terms <- terms
  }

  # Add additional information
  if (!is.null(x@misc$avgd.over)) {
    if (!identical(x@misc$avgd.over, character(0))) {
      output$averaged_over <- paste(x@misc$avgd.over, collapse = "; ")
    }
  }
  if (!is.null(x@misc$adjust)) {
    output$adjust <- x@misc$adjust
  }
  if (!is.null(x@misc$famSize  )) {
    output$family_size <- x@misc$famSize
  }

  # Add package information
  package <- list()

  package$name <- "emmeans"
  package$version <- getNamespaceVersion("emmeans")[[1]]

  # Add package information to output
  output$package <- package

  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'summary_emm'
#' @export
tidy_stats.summary_emm <- function(x, args = NULL) {
  output <- list()

  # Convert object to a data frame
  df <- as.data.frame(x)

  # Set method
  if ("contrast" %in% names(df)) {
    output$method <- "contrast"
  } else {
    output$method <- "EMM"
  }

  # Determine whether there are any groups
  pri_vars <- attr(x, "pri.vars")

  if (length(pri_vars) > 1) {
    # Create an empty groups list
    groups <- list()

    for (i in 1:length(levels(df[, 2]))) {
      # Create an empty group
      group <- list()

      # Set the group name
      name <- levels(df[, 2])[i]
      group$name <- name

      # Create an empty terms list
      terms <- list()

      # Filter the statistics of this group
      df_group <- df[df[2] == name, ]

      for (j in 1:nrow(df_group)) {

        # Create a new term list
        term <- list()

        # Add the name of the term
        term$name <- as.character(df_group$contrast[j])

        # Create a new statistics list and add the term's statistics
        statistics <- list()

        statistics$estimate$name <- "mean difference"
        statistics$estimate$value <- df_group$estimate[j]
        statistics$SE <- df_group$SE[j]
        statistics$df <- df_group$df[j]
        statistics$statistic$name <- "t"
        statistics$statistic$value <- df_group$t.ratio[j]
        statistics$p <- df_group$p.value[j]

        term$statistics <- statistics

        # Add the term data to the coefficients list
        terms[[j]] <- term
      }

      # Add coefficients to the group
      group$terms <- terms

      # Add group to the groups list
      groups[[i]] <- group
    }

    # Add groups to the output
    output$groups <- groups
  } else {
    # Create an empty terms list
    terms <- list()

    for (i in 1:nrow(df)) {
      # Create a new term list
      term <- list()

      # Add the name of the term
      term$name <- as.character(df$contrast[i])

      # Create a new statistics list and add the term's statistics
      statistics <- list()

      statistics$estimate$name <- "mean difference"
      statistics$estimate$value <- df$estimate[i]
      statistics$SE <- df$SE[i]
      statistics$df <- df$df[i]
      statistics$statistic$name <- "t"
      statistics$statistic$value <- df$t.ratio[i]
      statistics$p <- df$p.value[i]

      term$statistics <- statistics

      # Add the term data to the coefficients list
      terms[[i]] <- term
    }

    # Add terms to the output
    output$terms <- terms
  }

  # Set additional information
  mesg <- attr(x, "mesg")

  if (!is.null(mesg[1])) {
    output$df_method <- stringr::str_remove(mesg[1],
      "Degrees-of-freedom method: ")
  }
  if (!is.null(mesg[2])) {
    output$p_value_adjustment <- stringr::str_remove(mesg[2],
      "P value adjustment: ")
  }

  # Add package information
  package <- list()

  package$name <- "emmeans"
  package$version <- getNamespaceVersion("emmeans")[[1]]

  # Add package information to output
  output$package <- package

  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'emm_list'
#' @export
tidy_stats.emm_list <- function(x, args = NULL) {
  stop(paste("You're trying to tidy an object of class 'emm_list'; ",
    "please provide an object with class 'emmGrid'."))
}

#' @describeIn tidy_stats tidy_stats method for class 'icclist'
#' @export
tidy_stats.icclist <- function(x, args = NULL) {
  # Create the analysis list and set the method
  analysis <- list(method = "ICC")

  # Extract statistics
  statistics <- list()

  statistics <- add_statistic(statistics, "N subjects", x$subjects, "N",
    "subjects")
  statistics <- add_statistic(statistics, "N raters", x$raters, "N", "raters")
  statistics <- add_statistic(statistics, "ICC", x$value, interval = "CI",
    level = x$conf.level, lower = x$lbound, upper = x$ubound)
  statistics <- add_statistic(statistics, "statistic", x$Fvalue, "F")
  statistics <- add_statistic(statistics, "df numerator", x$df1, "df", "num.")
  statistics <- add_statistic(statistics, "df denominator", x$df2, "df", "den.")
  statistics <- add_statistic(statistics, "p", x$p.value)

  # Add statistics to the analysis
  analysis$statistics <- statistics

  # Add additional information
  analysis$model <- x$model
  analysis$type <- x$type
  analysis$unit <- x$unit
  analysis$ICC_name <- x$icc.name

  alternative <- list(null_value = x$r0)
  analysis$alernative <- alternative

  # Add package information
  analysis <- add_package_info(analysis, "irr")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'effsize'
#' @export
tidy_stats.effsize <- function(x, args = NULL) {
  # Create the analysis list and set the method
  analysis <- list(method = x$method)

  # Extract statistics
  statistics <- list()

  statistics <- add_statistic(statistics, x$name, x$estimate, interval = "CI",
    level = x$conf.level, lower = x$conf.int[["lower"]],
    upper = x$conf.int[["upper"]])

  # Add statistics to the analysis
  analysis$statistics <- statistics

  # Add package information
  analysis <- add_package_info(analysis, "effsize")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'effectsize_difference'
#' @export
tidy_stats.effectsize_difference <- function(x, args = NULL) {
  # Create the analysis list and set the method
  analysis <- list()

  # Determine the method
  method <- dplyr::case_when(
    "Cohens_d" %in% names(x) ~ "Cohen's d",
    "Hedges_g" %in% names(x) ~ "Hedge's g",
    "Glass_delta" %in% names(x) ~ "Glass' delta"
  )

  # Set the method
  analysis$method <- method

  # Extract statistics
  statistics <- list()

  # Special case: The name is equal to the method
  statistics <- add_statistic(statistics, method , x$Cohens_d,
    interval = "CI", level = x$CI, lower = x$CI_low, upper = x$CI_high)

  # Add statistics to the analysis
  analysis$statistics <- statistics

  # Add additional information
  alternative <- list(
    direction = attr(x, "alternative"),
    null_value = attr(x, "mu")
  )

  analysis$alternative <- alternative

  analysis$paired <- attr(x, "paired")
  analysis$correction <- attr(x, "correction")
  analysis$pooled_sd <- attr(x, "pooled_sd")
  analysis$proximate <- attr(x, "approximate")

  # Add package information
  analysis <- add_package_info(analysis, "effectsize")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'confint'
#' @export
tidy_stats.confint <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()

  # If there is only 1 parameter, set the name
  if (length(rownames(x)) == 1) {
    analysis$name <- rownames(x)[1]
  }

  # Set method
  analysis$method <- "Confidence intervals"

  # Extract confidence level
  CI_bounds <- readr::parse_number(colnames(x))
  CI_level <- (CI_bounds[2] - CI_bounds[1]) / 100

  # Check if there is 1 or more terms
  # If 1, only create a statistics list
  # If multiple, loop over terms and create separate lists for each term
  if (length(rownames(x)) == 1) {
    statistics <- list()

    statistics <- add_statistic(statistics, "lower", x[1])
    statistics <- add_statistic(statistics, "upper", x[2])

    analysis$statistics <- statistics
  } else {
    groups <- list(name = "Coefficients")

    for (i in 1:length(rownames(x))) {
      group <- list(name = rownames(x)[i])

      statistics <- list()
      statistics <- add_statistic(statistics, "lower", x[i, 1])
      statistics <- add_statistic(statistics, "upper", x[i, 2])

      group$statistics <- statistics

      groups$groups <- append(groups$groups, list(group))
    }

    analysis$groups <- append(analysis$groups, list(groups))
  }

  # Add additional information
  analysis$level <- CI_level

  # Add package information
  analysis <- add_package_info(analysis, "stats")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'psych'
#' @export
tidy_stats.psych <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()

  # Check the kind of psych object
  if ("alpha" %in% class(x)) {
    # Set method
    analysis$method <- "Reliability analysis"

    # Create a statistics list for the total statistics and CI
    statistics <- list()

    statistics <- add_statistic(statistics, "unstandardized alpha",
      x$total$raw_alpha, symbol = "α", subscript = "Σ")
    statistics <- add_statistic(statistics, "standardized alpha",
      x$total$std.alpha, symbol = "α", subscript = "R")
    statistics <- add_statistic(statistics, "Guttman's Lambda 6 reliability",
      x$total$`G6(smc)`, symbol = "Guttman's λ", subscript = "6")
    statistics <- add_statistic(statistics, "mean interitem correlation",
      x$total$average_r, symbol = "IIC", subscript = "M")
    statistics <- add_statistic(statistics, "signal-to-noise ratio",
      x$total$`S/N`, symbol = "S/N")
    statistics <- add_statistic(statistics, "standard error", x$total$ase,
      symbol = "SE")
    statistics <- add_statistic(statistics, "mean", x$total$mean,
      symbol = "M")
    statistics <- add_statistic(statistics, "standard deviation", x$total$sd,
      symbol = "SD")
    statistics <- add_statistic(statistics, "median interitem correlation",
      x$total$median_r, symbol = "IIC", subscript = "Mdn")

    # Add the statistics to the analysis
    analysis$statistics <- statistics

     # Create a group for the 95% confidence boundaries, if there are any
    if (!is.null(x$feldt) | !is.null(x$total$ase) | !is.null(x$boot.ci)) {
      group <- list(name = "95% confidence boundaries")

      if (!is.null(x$feldt)) {
        # Feldt group
        group_feldt <- list(name = "Feldt")
        statistics <- list()

        statistics <- add_statistic(statistics, "alpha", x$total$raw_alpha,
          symbol = "α", interval = "CI", level = .95,
          lower = x$feldt$lower.ci[[1]], upper = x$feldt$upper.ci[[1]])

        group_feldt$statistics <- statistics
        group$groups <- append(group$groups, list(group_feldt))
      }

      # Duhachek group
      if (!is.null(x$total$ase)) {
        group_duhachek <- list(name = "Duhachek")
        statistics <- list()

        statistics <- add_statistic(statistics, "alpha", x$total$raw_alpha,
          symbol = "α", interval = "CI", level = .95,
          lower = x$total$raw_alpha - 1.96 * x$total$ase,
          upper = x$total$raw_alpha + 1.96 * x$total$ase)

        group_duhachek$statistics <- statistics
        group$groups <- append(group$groups, list(group_duhachek))
      }

      # Bootstrapped group
      if (!is.null(x$boot.ci)) {
        group_bootstrapped <- list(name = "bootstrapped")
        statistics <- list()

        statistics <- add_statistic(statistics, "alpha", x$boot.ci[2],
          symbol = "α", interval = "CI", level = .95,
          lower = x$boot.ci[1], upper = x$boot.ci[3])

        group_bootstrapped$statistics <- statistics
        group$groups <- append(group$groups, list(group_bootstrapped))
      }

      analysis$groups <- append(analysis$groups, list(group))
    }

    # Create a group for the the reliability if an item is dropped statistics
    group <- list(name = "Reliability if an item is dropped")

    # Loop over the items
    for (i in 1:nrow(x$alpha.drop)) {
      # Create a list for the item
      item <- list(name = rownames(x$alpha.drop)[i])

      # Create a statistics list and add the item statistics
      statistics <- list()

      statistics <- add_statistic(statistics, "unstandardized alpha",
        x$total$raw_alpha[i], symbol = "α", subscript = "Σ")
      statistics <- add_statistic(statistics, "standardized alpha",
        x$total$std.alpha[i], symbol = "α", subscript = "R")
      statistics <- add_statistic(statistics, "Guttman's Lambda 6 reliability",
        x$total$`G6(smc)`, symbol = "Guttman's λ", subscript = "6")
      statistics <- add_statistic(statistics, "mean interitem correlation",
        x$total$average_r[i], symbol = "IIC", subscript = "M")
      statistics <- add_statistic(statistics, "signal-to-noise ratio",
        x$total$`S/N`[i], symbol = "S/N")
      statistics <- add_statistic(statistics, "standard error", x$total$ase[i],
        symbol = "SE")
      statistics <- add_statistic(statistics, "mean", x$total$mean[i],
        symbol = "M")
      statistics <- add_statistic(statistics, "standard deviation",
        x$total$sd[i], symbol = "SD")
      statistics <- add_statistic(statistics, "variance interitem correlation",
        x$total$var.r[i], symbol = "IIC", subscript = "var")
      statistics <- add_statistic(statistics, "median interitem correlation",
        x$total$med.r[i], symbol = "IIC", subscript = "Mdn")

      # Add statistics to the group
      item$statistics <- statistics

      # Add item to the group
      group$groups <- append(group$groups, list(item))
    }

    # Add the reliability if an item is dropped group to the analysis groups
    analysis$groups <- append(analysis$groups, list(group))

    # Create a group for the item statistics
    group <- list(name = "Item statistics")

    # Loop over the items
    for (i in 1:nrow(x$item.stats)) {
      # Create a list for the item
      item <- list(name = rownames(x$item.stats)[i])

      # Create a statistics list and add the item statistics
      statistics <- list()

      statistics <- add_statistic(statistics, "number of complete cases",
        x$item.stats$n[i], symbol = "n")
      statistics <- add_statistic(statistics,
        "total score correlation with standardized items",
        x$item.stats$r[i], symbol = "r", subscript = "std")
      statistics <- add_statistic(statistics, "total score correlation",
        x$item.stats$raw.r[i], symbol = "r", subscript = "raw")
      statistics <- add_statistic(statistics,
        "total score correlation with standardized items",
        x$item.stats$std.r[i], symbol = "r", subscript = "std")
      statistics <- add_statistic(statistics,
        "corrected item whole correlation",
        x$item.stats$r.cor[i], symbol = "r", subscript = "cor")
      statistics <- add_statistic(statistics,
        "item whole correlation without this item",
        x$item.stats$r.drop[i], symbol = "r", subscript = "drop")
      statistics <- add_statistic(statistics, "mean", x$item.stats$mean[i],
        symbol = "M")
      statistics <- add_statistic(statistics, "standard deviation",
        x$item.stats$sd[i], symbol = "SD")

      # Add statistics to the group
      item$statistics <- statistics

      # Add item to the group
      group$groups <- append(group$groups, list(item))
    }

    # Add the reliability if an item is dropped group to the analysis groups
    analysis$groups <- append(analysis$groups, list(group))
  }

  if ("corr.test" %in% class(x)) {
    analysis$method <- "Correlation" #TODO detect kendall or spearman

    if (x$adjust == "none") {
      warning("Only saving unadjusted statistics.")
    } else {
      warning("Only saving adjusted statistics.")
    }

    # Check if there is only 1 pair, or multiple
    if (length(rownames(x$r)) == 1) {
      # Create a list for the statistics of this single pair
      statistics <- list()

      #TODO: figure out number of pairs

    }

    analysis$adjust <- x$adjust
  }

  if ("mardia" %in% class(x)) {
     # Set method
    analysis$method <- "Mardia's test"

    # Create a statistics list for number of observations and variables
    statistics <- list()

    statistics <- add_statistic(statistics, "number of observations", x$n.obs,
      symbol = "N")
    statistics <- add_statistic(statistics, "number of variables", x$n.var,
      symbol = "k")

    analysis$statistics <- statistics

    # Create a group for the skew statistics
    group <- list(name = "skew")

    statistics <- list()

    statistics <- add_statistic(statistics, "estimate", x$b1p, symbol = "b",
      subscript = "1, p")
    statistics <- add_statistic(statistics, "skew", x$skew)
    statistics <- add_statistic(statistics, "p", x$p.skew)

    group$statistics <- statistics
    analysis$groups <- append(analysis$groups, list(group))

    # Create a group for the small sample skew statistics
    group <- list(name = "small sample skew")

    statistics <- list()

    statistics <- add_statistic(statistics, "estimate", x$b1p, symbol = "b",
      subscript = "1, p")
    statistics <- add_statistic(statistics, "skew", x$small.skew)
    statistics <- add_statistic(statistics, "p", x$p.small)

    group$statistics <- statistics
    analysis$groups <- append(analysis$groups, list(group))

    # Create a group for the kurtosis statistics
    group <- list(name = "kurtosis")

    statistics <- list()

    statistics <- add_statistic(statistics, "estimate", x$b2p, symbol = "b",
      subscript = "2, p")
    statistics <- add_statistic(statistics, "kurtosis", x$kurtosis)
    statistics <- add_statistic(statistics, "p", x$p.kurt)

    group$statistics <- statistics
    analysis$groups <- append(analysis$groups, list(group))
  }

  # Add package information
  analysis <- add_package_info(analysis, "psych")

  return(analysis)
}
