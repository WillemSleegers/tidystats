#' @describeIn tidy_stats tidy_stats method for class 'lavaan'
tidy_stats.lavaan <- function(x, args = list(fit.measures = TRUE)) {
  output <- list()

  # Set method
  output$method <- "SEM"

  # Get summary statistics
  sink("file") # Use this hack to hide the default printout of summary()
  summary <- do.call(get("summary", asNamespace("lavaan")), c(list(x), args))
  sink()

  # Create a statistics list
  statistics <- list()
  statistics$n_parameters <- x@Fit@npar
  statistics$N <- x@Data@nobs[[1]]

  # Create an empty models list
  models <- list()

  # Add user model
  model <- list()
  model$name <- "user model"
  model$statistics$statistic$name <- "X-squared"
  model$statistics$statistic$value <- x@test$standard$stat
  model$statistics$df <- x@test$standard$df
  model$statistics$p <- x@test$standard$pvalue
  models[[1]] <- model

  # Get fit statistics
  if (!is.null(args$fit.measures)) {
    if (args$fit.measures) {
      fit <- summary$FIT

      statistics$CFI <- fit["cfi"]
      statistics$TLI <- fit["tli"]
      statistics$log_likelihood <- fit["logl"]
      statistics$log_likelihood_unrestricted <- fit["unrestricted.logl"]
      statistics$AIC <- fit["aic"]
      statistics$BIC <- fit["bic"]
      statistics$BIC_adjusted <- fit["bic2"]
      statistics$RMSEA <- fit["rmsea"]
      statistics$CI$CI_level <- .90
      statistics$CI$CI_lower <- fit["rmsea.ci.lower"]
      statistics$CI$CI_upper <- fit["rmsea.ci.upper"]
      statistics$p <- fit["rmsea.pvalue"]
      statistics$SRMR <- fit["srmr"]

      # Add baseline model
      model <- list()
      model$name <- "baseline model"
      model$statistics$statistic$name <- "X-squared"
      model$statistics$statistic$value <- fit["baseline.chisq"]
      model$statistics$df <- fit["baseline.df"]
      model$statistics$p <- fit["baseline.pvalue"]
      models[[2]] <- model
    }
  }

  # Add statistics and model to output
  output$statistics <- statistics
  output$models <- models

  # Extract PE from the summary
  PE <- summary$PE

  # Create a groups list and loop over each group
  groups <- list()

  # Set grouped_by
  output$grouped_by <- x@Data@group

  for (i in 1:x@Data@ngroups) {
    group <- list()

    group$name <- x@Data@group.label[i]

    # Latent variables
    # Create an empty list for the latent variables
    latent_variables <- list()

    # Select only the latent variables statistics from the PE data
    PE_latent <- dplyr::filter(PE, op == "=~")

    # Loop
    for (j in 1:nrow(PE_latent)) {
      var <- PE_latent[j, ]

      latent_variable <- list()
      latent_variable$name <- paste(var$lhs, var$op, var$rhs)
      latent_variable$statistics$estimate$name <- "b"
      latent_variable$statistics$estimate$value <- var$est
      latent_variable$statistics$SE <- var$se

      if (!is.na(var$z)) {
        latent_variable$statistics$statistic$name <- "z"
        latent_variable$statistics$statistic$value <- var$z
        latent_variable$statistics$p <- var$p
      }

      if (!is.null(args$standardized)) {
        if (args$standardized) {
          latent_variable$statistics$std_lv <- var$std.lv
          latent_variable$statistics$std_all <- var$std.all
          latent_variable$statistics$std_nox <- var$std.nox
        }
      }

      if (!is.null(args$ci)) {
        if (args$ci) {
          latent_variable$statistics$CI$CI_level <- .90
          latent_variable$statistics$CI$CI_lower <- var$ci.lower
          latent_variable$statistics$CI$CI_upper <- var$ci.upper
        }
      }

      latent_variables[[j]] <- latent_variable
    }

    # Add latent variables to output
    group$latent_variables <- latent_variables

    # Regressions
    # Create an empty list for the regressions
    regressions <- list()

    # Select only the regressions statistics from the PE data
    PE_regressions <- dplyr::filter(PE, op == "~")

    # Loop, if there are any regressions
    if (nrow(PE_regressions) > 0) {
      for (j in 1:nrow(PE_regressions)) {
        reg <- PE_regressions[j, ]

        regression <- list()
        regression$name <- paste(reg$lhs, reg$op, reg$rhs)
        regression$statistics$estimate$name <- "b"
        regression$statistics$estimate$value <- reg$est
        regression$statistics$SE <- reg$se
        regression$statistics$statistic$name <- "z"
        regression$statistics$statistic$value <- reg$z
        regression$statistics$p <- reg$p

        if (!is.null(args$standardized)) {
          if (args$standardized) {
            regression$statistics$std_lv <- reg$std.lv
            regression$statistics$std_all <- reg$std.all
            regression$statistics$std_nox <- reg$std.nox
          }
        }

        if (!is.null(args$ci)) {
          if (args$ci) {
            regression$statistics$CI$CI_level <- .90
            regression$statistics$CI$CI_lower <- reg$ci.lower
            regression$statistics$CI$CI_upper <- reg$ci.upper
          }
        }

        regressions[[j]] <- regression
      }

      # Add regressions to output
      group$regressions <- regressions
    }

    # Covariances
    # Create an empty list for the covariances
    covariances <- list()

    # Select only the covariance statistics from the PE data
    PE_covariances <- dplyr::filter(PE, op == "~~" & lhs != rhs)

    # Loop
    for (j in 1:nrow(PE_covariances)) {
      covar <- PE_covariances[j, ]

      covariance <- list()
      covariance$name <- paste(covar$lhs, covar$op, covar$rhs)
      covariance$statistics$estimate$name <- "b"
      covariance$statistics$estimate$value <- covar$est
      covariance$statistics$SE <- covar$se
      covariance$statistics$statistic$name <- "z"
      covariance$statistics$statistic$value <- covar$z
      covariance$statistics$p <- covar$p

      if (!is.null(args$standardized)) {
        if (args$standardized) {
          covariance$statistics$std_lv <- covar$std.lv
          covariance$statistics$std_all <- covar$std.all
          covariance$statistics$std_nox <- covar$std.nox
        }
      }

      if (!is.null(args$ci)) {
        if (args$ci) {
          covariance$statistics$CI$CI_level <- .90
          covariance$statistics$CI$CI_lower <- covar$ci.lower
          covariance$statistics$CI$CI_upper <- covar$ci.upper
        }
      }

      covariances[[j]] <- covariance
    }

    # Add covariances to output
    group$covariances <- covariances

    # Intercepts
    # Create an empty list for the covariances
    intercepts <- list()

    # Select only the covariance statistics from the PE data
    PE_intercepts <- dplyr::filter(PE, op == "~1" & lhs != rhs)

    # Loop, if there are any intercepts
    if (nrow(PE_regressions) > 0) {
      for (j in 1:nrow(PE_intercepts)) {
        inter <- PE_intercepts[j, ]

        intercept <- list()
        intercept$name <- paste(inter$lhs, inter$op, inter$rhs)
        intercept$statistics$estimate$name <- "b"
        intercept$statistics$estimate$value <- inter$est
        intercept$statistics$SE <- inter$se
        intercept$statistics$statistic$name <- "z"
        intercept$statistics$statistic$value <- inter$z
        intercept$statistics$p <- inter$p

        if (!is.null(args$standardized)) {
          if (args$standardized) {
            intercept$statistics$std_lv <- inter$std.lv
            intercept$statistics$std_all <- inter$std.all
            intercept$statistics$std_nox <- inter$std.nox
          }
        }

        if (!is.null(args$ci)) {
          if (args$ci) {
            intercept$statistics$CI$CI_level <- .90
            intercept$statistics$CI$CI_lower <- inter$ci.lower
            intercept$statistics$CI$CI_upper <- inter$ci.upper
          }
        }

        intercepts[[j]] <- intercept
      }

      # Add intercepts to output
      group$intercepts <- intercepts
    }

    # Variances
    # Create an empty list for the variances
    variances <- list()

    # Select only the variance statistics from the PE data
    PE_variances <- dplyr::filter(PE, lhs == rhs)

    # Loop
    for (j in 1:nrow(PE_variances)) {
      var <- PE_variances[j, ]

      variance <- list()
      variance$name <- var$lhs
      variance$statistics$estimate$name <- "b"
      variance$statistics$estimate$value <- var$est
      variance$statistics$SE <- var$se
      variance$statistics$statistic$name <- "z"
      variance$statistics$statistic$value <- var$z
      variance$statistics$p <- var$p

      if (!is.null(args$standardized)) {
        if (args$standardized) {
          variance$statistics$std_lv <- var$std.lv
          variance$statistics$std_all <- var$std.all
          variance$statistics$std_nox <- var$std.nox
        }
      }

      if (!is.null(args$ci)) {
        if (args$ci) {
          variance$statistics$CI$CI_level <- .90
          variance$statistics$CI$CI_lower <- var$ci.lower
          variance$statistics$CI$CI_upper <- var$ci.upper
        }
      }

      variances[[j]] <- variance
    }

    # Add variances to output
    group$variances <- variances
  }

  # Add group to groups
  groups[[i]] <- group

  # If there are no groups, add the information directly to the output instead
  if (x@Model@ngroups == 1) {
    output$latent_variables <- latent_variables

    if (nrow(PE_regressions) > 0) {
      output$regressions <- regressions
    }

    output$covariances <- covariances

    if (nrow(PE_intercepts) > 0) {
      output$intercepts <- intercepts
    }

    output$variances <- variances
  } else {
    # Add groups to output
    output$groups <- groups
  }

  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'psych'
#' @export
tidy_stats.psychwip <- function(x, args = NULL) {
  output <- list()

  # Check the kind of psych object
  if ("corr.test" %in% class(x)) {
    output$method <- "Correlations"

    # Create an empty pairs list
    pairs <- list()

    # Extract variable names
    rownames <- rownames(x$r)
    colnames <- colnames(x$r)

    # Determine whether the correlation matrix is symmetric or asymmetric
    if (identical(rownames, colnames)) {
      I <- choose(length(rownames), 2)
      symmetric <- TRUE
    } else {
      I <- length(rownames) * length(colnames)
      symmetric <- FALSE
    }

    # Tidy statistics
    rs <- tidy_matrix(x$r, symmetric = symmetric)
    SEs <- tidy_matrix(x$se, symmetric = symmetric)
    ts <- tidy_matrix(x$t, symmetric = symmetric)
    ps <- tidy_matrix(t(x$p), symmetric = symmetric)
    ps_adjusted <- tidy_matrix(x$p, symmetric = symmetric)

    # Check if there are confidence intervals
    if (!is.null(x$ci)) {
      # Figure out the level
      alpha <- as.character(x$Call)[which(names(x$Call) == "alpha")]

      if (length(alpha) == 0) {
        level <- .95
      } else {
        level <- 1 - as.numeric(alpha)
      }
    }

    if (length(x$n) == 1) {
      n <- x$n
    } else {
      ns <- tidy_matrix(x$n, symmetric = symmetric)
    }

    # Loop over the pairs
    for (i in 1:I) {
      pair <- list()

      # Set names
      names <- list()
      names[[1]] <- rs$name1[i]
      names[[2]] <- rs$name2[i]
      pair$names <- names

      # Set statistics
      if (length(x$n) == 1) {
        pair$statistics$n <- n
      } else {
        pair$statistics$n <- ns$value[i]
      }
      pair$statistics$estimate$name <- "r"
      pair$statistics$estimate$value <- rs$value[i]
      pair$statistics$SE <- SEs$value[i]
      pair$statistics$statistic$name <- "t"
      pair$statistics$statistic$value <- ts$value[i]
      pair$statistics$p <- ps$value[i]

      if (x$adjust != "none") {
        pair$statistics$p_adjusted <- ps_adjusted$value[i]
      }

      if (!is.null(x$ci)) {
        pair$statistics$CI$CI_level <- level
        pair$statistics$CI$CI_lower <- x$ci$lower[i]
        pair$statistics$CI$CI_upper <- x$ci$upper[i]
      }

      if (!is.null(x$ci)) {
        pair$statistics$CI_adjusted$CI_level <- level
        pair$statistics$CI_adjusted$CI_lower <- x$ci.adj$lower[i]
        pair$statistics$CI_adjusted$CI_upper <- x$ci.adj$upper[i]
      }

      pairs[[i]] <- pair
    }

    # Add pairs to output
    output$pairs <- pairs

    # Set multiple test adjustment method
    output$multiple_test_adjustment <- x$adjust
  } else {
    stop("Sorry, tidystats does not (yet) support this kind of analysis.")
  }

  return(output)
}
