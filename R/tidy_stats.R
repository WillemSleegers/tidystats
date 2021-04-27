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

  output <- list()

  # Extract method
  method <- x$method
  
  # Extract number of simulations from Fisher's test based on simulated p-values
  if (stringr::str_detect(method, "simulated p-value")) {
    output$method <- "Fisher's Exact Test for Count Data with simulated p-value"

    output$sim <- as.numeric(stringr::str_extract(method, 
      "[0-9](e\\+)?([0-9].)?"))
  # Extract parameters from Fisher's test using sym. chisq
  } else if (stringr::str_detect(method, "hybrid using asym")) {
    output$method <- paste("Fisher's Exact Test for Count Data",
      "hybrid using asym.chisq")

    hybridPars <- list()

    hybridPars$expect = readr::parse_number(stringr::str_extract(method,
      "exp=[0-9+]"))
    hybridPars$percent = readr::parse_number(stringr::str_extract(method,
      "perc=[0-9+]"))
    hybridPars$Emin = readr::parse_number(stringr::str_extract(method,
      "exp=[0-9+]"))

    output$hybrid_parameters <- hybridPars
  } else if (stringr::str_detect(method, "Two Sample t-test")) {
    # (use trimws to remove the leading space from a Two Sample t-test)
    method <- trimws(method)
    output$method <- method
    
    if (stringr::str_detect(method, "Welch")) {
      output$var_equal <- FALSE
    } else {
      output$var_equal <- TRUE
    }
    
  } else if (stringr::str_detect(method, 
      "One-way analysis of means \\(not assuming equal variances\\)")) {
    output$method <- "One-way analysis of means"
    output$var_equal <- FALSE
  } else {
    output$method <- method
    output$var_equal <- TRUE
  }

  # Extract DV and IV information
  output$name <- x$data.name
  
  # Extract statistics
  statistics <- list()

  # Estimate
  if (!is.null(x$estimate)) {
    estimate <- list()
    
    # Special case: Calculate estimate for Two Sample t-tests
     if (length(x$estimate) > 1) {
      estimate_name <- "mean difference"
      estimate_value <- x$estimate[[1]] - x$estimate[[2]]
    } else {
      estimate_name <- names(x$estimate)
      estimate_value <- x$estimate[[1]]
    }

    estimate$name <- dplyr::case_when(
      x$method == "One Sample t-test" ~ "mean",
      x$method == "Paired t-test" ~ "mean difference",
      TRUE ~ estimate_name
    )
    estimate$value <- estimate_value
    
    statistics$estimate <- estimate 
  }
  
  # SE
  if (!is.null(x$stderr)) {
    statistics$SE <- x$stderr
  }
  
  # Test statistic
  if (!is.null(x$statistic)) {
    statistic <- list()
    statistic$name <- names(x$statistic)
    statistic$value <- x$statistic[[1]]
    statistics$statistic <- statistic
  }
  
  # Degrees of freedom
  # Special case: One-way analysis of means without equal variance assumption
  if (length(x$parameter) > 1) {
    dfs <- list()
    dfs$df_numerator <- x$parameter[[1]]
    dfs$df_denominator <- x$parameter[[2]]
    statistics$dfs <- dfs
  } else {
    statistics$df <- x$parameter[[1]]
  }
  
  # p-value
  statistics$p <- x$p.value
  
  # Extract confidence intervals
  if (!is.null(x$conf.int)) {
    CIs <- list()

    CIs$CI_level <- attr(x$conf.int, "conf.level")
    CIs$CI_lower <- x$conf.int[1]
    CIs$CI_upper <- x$conf.int[2]

    statistics$CI <- CIs
  }

  # Add statistics to output
  output$statistics <- statistics
  
  # Add alternative hypothesis information
  if (!is.null(x$alternative)) {
    alternative <- list()

    alternative$direction <- x$alternative
    alternative$null_value <- x$null.value[[1]]

    # Add alternative hypothesis information to output
    output$alternative <- alternative
  }

  # Add package information
  package <- list()

  package$name <- "stats"
  package$version <- getNamespaceVersion("stats")[[1]]

  # Add package information to output
  output$package <- package

  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'lm'
#' @export
tidy_stats.lm <- function(x, args = NULL) {

  output <- list()
  
  # Get summary statistics
  summary <- summary(x)

  # Extract method
  output$method <- "Linear regression"
  
  # Extract model statistics
  statistics <- list()
  
  statistics$r_squared <- summary$r.squared
  statistics$r_squared_adjusted <- summary$adj.r.squared
  
  statistic <- list()
  statistic$name <- "F"
  statistic$value <- summary$fstatistic[[1]]
  statistics$statistic <- statistic
  
  dfs <- list()
  dfs$df_numerator <- summary$fstatistic[[2]]
  dfs$df_denominator <- summary$fstatistic[[3]]
  statistics$dfs <- dfs
  
  statistics$p <- stats::pf(summary$fstatistic[[1]], summary$fstatistic[[2]], 
    summary$fstatistic[[3]], lower.tail = FALSE)
  statistics$sigma <- summary$sigma
  
  # Add statistics to output
  output$statistics <- statistics
  
  # Extract statistics of the coefficients, although we will call them 'terms'
  coefficients <- stats::coef(summary)
  
  # Create an empty terms list
  terms <- list()
  
  for (i in 1:nrow(coefficients)) {
    
    # Create a new term list
    term <- list()
    
    # Add the name of the term
    name = rownames(coefficients)[i]
    term$name <- name
    
    # Create a new statistics list and add the term's statistics
    statistics <- list()
    
    estimate <- list()
    estimate$name <- "b"
    estimate$value <- coefficients[i, "Estimate"]
    statistics$estimate <- estimate
    
    statistics$SE <- coefficients[i, "Std. Error"]
    
    statistic <- list()
    statistic$name <- "t"
    statistic$value <- coefficients[i, "t value"]
    statistics$statistic <- statistic
    
    statistics$df <- summary$df[2]
    
    statistics$p <- coefficients[i, "Pr(>|t|)"]
    
    term$statistics <- statistics
    
    # Add the term data to the terms list
    terms[[i]] <- term
  }
  
  # Add terms to output
  output$terms <- terms
  
  # Add package information
  package <- list()

  package$name <- "stats"
  package$version <- getNamespaceVersion("stats")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'glm'
#' @export
tidy_stats.glm <- function(x, args = NULL) {

  output <- list()
  
  # Get summary statistics
  summary <- summary(x)

  # Extract method
  output$method <- "Generalized linear regression"
  
  # Extract model statistics
  statistics <- list()
  
  statistics$deviance_null <- summary$null.deviance
  statistics$deviance_residual <- summary$deviance
  
  dfs <- list()
  dfs$df_null <- summary$df.null
  dfs$df_residual <- summary$df.residual
  statistics$dfs <- dfs
  
  statistics$AIC <- summary$aic
  
  # Add model fit statistics to output
  output$statistics <- statistics
  
  # Extract statistics of the coefficients, although we will call them 'terms'
  coefficients <- stats::coef(summary)
  
  # Create an empty terms list
  terms <- list()
  
  for (i in 1:nrow(coefficients)) {
    
    # Create a new term list
    term <- list()
    
    # Add the name of the term
    term$name <- rownames(coefficients)[i]
    
    # Create a new statistics list and add the term's statistics
    statistics <- list()
    
    estimate <- list()
    estimate$name <- "b"
    estimate$value <- coefficients[i, "Estimate"]
    statistics$estimate <- estimate
    
    statistics$SE <- coefficients[i, "Std. Error"]
    
    statistic <- list()
    statistic$name <- ifelse(colnames(coefficients)[3] == "z value", "z", "t")
    statistic$value <- coefficients[i, 3]
    statistics$statistic <- statistic
    
    statistics$df <- summary$df[2]
    
    statistics$p <- coefficients[i, 4]
    
    term$statistics <- statistics
    
    # Add the term data to the terms list
    terms[[i]] <- term
  }
  
  # Add terms to output
  output$terms <- terms
  
  # Add additional information
  if (!is.null(summary$iter)) {
    output$iterations <- summary$iter
  }
  
  if (!is.null(summary$dispersion)) {
    output$dispersion <- summary$dispersion
  }
  
  # Add package information
  package <- list()

  package$name <- "stats"
  package$version <- getNamespaceVersion("stats")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'anova'
#' @export
tidy_stats.anova <- function(x, args = NULL) {
  
  output <- list()
  
  # Create an empty terms list
  terms <- list()
  
  # Figure out whether it's an ANOVA or ANODE
  heading = attr(x, "heading")
  #TODO: Extract error distribution
  #TODO: Extract link function
  
  # Convert the summary statistics format to a data frame
  x <- tibble::as_tibble(x, rownames = "terms")
  
  # Determine method
  if ("deviance" %in% tolower(names(x))) {
    output$method <- "ANODE"
  } else {
    output$method <- "ANOVA"
  }
  
  # Trim spaces from the names of the terms
  x <- dplyr::mutate(x, terms = stringr::str_trim(terms))
  
  for (i in 1:nrow(x)) {
    # Create a new term list
    term <- list()
    
    # Add the name of the term
    name <- x$terms[i]
    term$name <- name
    
    # Create a new statistics list and add the term's statistics
    statistics <- list()
    
    if ("npar" %in% colnames(x)) {statistics$n_parameters <- x$npar[i]}
    if ("AIC" %in% colnames(x)) {statistics$AIC <- x$AIC[i]}
    if ("BIC" %in% colnames(x)) {statistics$BIC <- x$BIC[i]}
    if ("logLik" %in% colnames(x)) {statistics$log_likelihood <- x$logLik[i]}
    if ("deviance" %in% colnames(x)) {
      if (!is.na(x$deviance[i])) {
        statistics$deviance = x$deviance[i]  
      }
    }
    if ("Deviance" %in% colnames(x)) {
      if (!is.na(x$Deviance[i])) {
        statistics$deviance = x$Deviance[i]  
      }
    }
    if ("Resid. Dev" %in% colnames(x)) {
      if (!is.na(x$`Resid. Dev`[i])) {
        statistics$deviance_residual = x$`Resid. Dev`[i]
      }
    }
    if ("RSS" %in% colnames(x)) {statistics$RSS <- x$RSS[i]}
    if ("Sum Sq" %in% colnames(x)) {statistics$SS <- x$`Sum Sq`[i]}
    if ("Sum of Sq" %in% colnames(x)) {
      if (!is.na(x$`Sum of Sq`[i])) {
        statistics$SS <- x$`Sum of Sq`[i]
      }
    }
    if ("Mean Sq" %in% colnames(x)) {statistics$MS <- x$`Mean Sq`[i]}
    if ("Chisq" %in% colnames(x)) {
      if (!is.na(x$Chisq[i])) {
        statistic <- list()
        statistic$name <- "X-squared"
        statistic$value <- x$Chisq[i]
        statistics$statistic <- statistic
      }
    }
    if ("F value" %in% colnames(x)) {
      if (!is.na(x$`F value`[i])) {
        statistic <- list()
        statistic$name <- "F"
        statistic$value <- x$`F value`[i]
        statistics$statistic <- statistic
      }
    }
    if ("F" %in% colnames(x)) {
      if (!is.na(x$F[i])) {
        statistic <- list()
        statistic$name <- "F"
        statistic$value <- x$F[i]
        statistics$statistic <- statistic
      }
    }
    if (name != "Residuals") {
      if (x$terms[nrow(x)] == "Residuals") {
        dfs <- list()
        dfs$df_numerator <- x$Df[i]
        dfs$df_denominator <- x$Df[nrow(x)]
        statistics$dfs <- dfs
      } else {
        if ("Df" %in% colnames(x)) {
          if (!is.na(x$Df[i])) {
            if (!"Res.Df" %in% colnames(x) & !"Resid. Df" %in% colnames(x)) {
              statistics$df <- x$Df[i]
            } else {
              dfs <- list()
              dfs$df <- x$Df[i] # Call this df_numerator?
              if ("Res.Df" %in% colnames(x)) {
                dfs$df_residual <- x$Res.Df[i] # Call this df_denominator?
              }
              if ("Resid. Df" %in% colnames(x)) {
                dfs$df_residual <- x$`Resid. Df`[i] # Call this df_denominator?
              }
              statistics$dfs <- dfs 
            }
          } else {
            if ("Res.Df" %in% colnames(x)) {statistics$df <- x$Res.Df[i]}
            if ("Resid. Df" %in% colnames(x)) {
              statistics$df <- x$`Resid. Df`[i]
            }
          } 
        }
        if ("NumDF" %in% colnames(x)) {
          statistics$dfs$df_numerator <- x$NumDF[i]
          statistics$dfs$df_denominator <- x$DenDF[i]
        }
      }
    } else {
      if ("Df" %in% colnames(x)) {
        statistics$df <- x$Df[i]
      }
    }
    if ("Rao" %in% colnames(x)) {
      if (!is.na(x$Rao[i])) {
        statistics$rao <- x$Rao[i]
      }
    }
    if ("Pr(>F)" %in% colnames(x)) {
      if (!is.na(x$`Pr(>F)`[i])) {
        statistics$p <- x$`Pr(>F)`[i]
      }
    }
    if ("Pr(>Chi)" %in% colnames(x)) {
      if (!is.na(x$`Pr(>Chi)`[i])) {
        statistics$p <- x$`Pr(>Chi)`[i]
      }
    }
    if ("Pr(>Chisq)" %in% colnames(x)) {
      if (!is.na(x$`Pr(>Chisq)`[i])) {
        statistics$p <- x$`Pr(>Chisq)`[i]
      }
    }
    if ("Cp" %in% colnames(x)) {
      statistics$cp <- x$Cp[i]
    }
    
    # Add statistics to the term
    term$statistics <- statistics
    
    # Add the term data to the coefficients list
    terms[[i]] <- term
  }
  
  # Add terms to the output
  output$terms <- terms
  
  # Add package information
  package <- list()

  package$name <- "stats"
  package$version <- getNamespaceVersion("stats")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'aov'
#' @export
tidy_stats.aov <- function(x, args = NULL) {

  output <- list()
  
  # Get summary statistics
  summary <- summary(x)

  # Extract method
  output$method <- "ANOVA"
  
  # Create an empty terms list
  terms <- list()
  
  # Convert the summary statistics format to a data frame
  summary <- tibble::as_tibble(summary[[1]], rownames = "terms")
  
  # Trim spaces from the names of the terms
  summary <- dplyr::mutate(summary, terms = stringr::str_trim(terms))
  
  for (i in 1:nrow(summary)) {
    
    # Create a new term list
    term <- list()
    
    # Add the name of the term
    name = summary$terms[i]
    term$name <- name
    
    # Create a new statistics list and add the term's statistics
    statistics <- list()
    
    statistics$SS <- summary$`Sum Sq`[i]
    statistics$MS <- summary$`Mean Sq`[i]
    
    if (name != "Residuals") {
      statistics$statistic$name <- "F"
      statistics$statistic$value <- summary$`F value`[i]
      
      statistics$dfs$df_numerator <- summary$Df[i]
      statistics$dfs$df_denominator <- summary$Df[nrow(summary)]
    } else {
      statistics$df <- summary$Df[i]
    }
    
    if (name != "Residuals") {
      statistics$p <- summary$`Pr(>F)`[i]
    }
    
    term$statistics <- statistics
    
    # Add the term data to the coefficients list
    terms[[i]] <- term
  }
  
  # Add coefficients to the output
  output$terms <- terms
  
  # Add package information
  package <- list()

  package$name <- "stats"
  package$version <- getNamespaceVersion("stats")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'aovlist'
#' @export
tidy_stats.aovlist <- function(x, args = NULL) {

  output <- list()
  
  # Get summary statistics
  summary <- summary(x)

  # Extract method
  output$method <- "ANOVA"
  
  # Create an empty groups list
  groups <- list()
  
  for (i in 1:length(summary)) {
    
    # Create an empty group
    group <- list()
    
    # Set the group name
    group$name <- stringr::str_remove(names(summary[i]), "Error: ")
    
    # Create an empty terms list
    terms <- list()
    
    # Convert the summary statistics format to a data frame
    summary_df <- tibble::as_tibble(as.data.frame(as.list.data.frame(
      summary[[i]])), rownames = "terms")
    
    # Trim spaces from the names of the terms
    summary_df <- dplyr::mutate(summary_df, terms = stringr::str_trim(terms))
    
    for (j in 1:nrow(summary_df)) {
    
      # Create a new term list
      term <- list()
      
      # Add the name of the term
      name = summary_df$terms[j]
      term$name <- name
      
      # Create a new statistics list and add the term's statistics
      statistics <- list()
      
      statistics$SS <- summary_df$`Sum.Sq`[j]
      statistics$MS <- summary_df$`Mean.Sq`[j]
      
      if (name != "Residuals") {
        statistic <- list()
        statistic$name <- "F"
        statistic$value <- summary_df$`F.value`[j]
        statistics$statistic <- statistic
      }
      
      statistics$df <- summary_df$Df[j]
      
      if (name != "Residuals") {
        statistics$p <- summary_df$Pr..F.[j]
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
 
  # Add package information
  package <- list()

  package$name <- "stats"
  package$version <- getNamespaceVersion("stats")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'tidystats_descriptives'
#' @export
tidy_stats.tidystats_descriptives <- function(x, args = NULL) {

  output <- list()
  
  # Add method
  output$method <- "Descriptives"
  
  # Extract variable information
  var_names <- unique(dplyr::pull(x, var))
  
  # Extract grouping information
  group_names <- dplyr::group_vars(x)
  n_groups <- length(group_names)
  
  # Loop over the variable names
  variables <- list()
  
  for (i in 1:length(var_names)) {
    variable <- list()
    
    # Subset the data frame to only contain the rows of the current variable
    subset <- dplyr::filter(x, var == var_names[i])
    
    # Set the variable name
    variable$name <- var_names[i]
    
    # Extract statistics
    # Check whether there are any grouping variables and select the relevant
    # row of descriptives
    if (n_groups > 0) {
      
      # Set the grouping name
      # If there is more than 1 grouping variable, combine them together
      variable$group_by = paste(group_names, collapse = " by ")
      
      # Create an empty groups list
      groups <- list()
      
      # Loop over the groups
      for (j in 1:nrow(subset)) {
        # Create an empty group list
        group <- list()
        
        # Select the current row
        row <- x[j, ]
        
        # Set the group name
        group$name <- paste(unlist(row[group_names]), collapse = " - ")
        
        # Extract statistics
        statistics <- list()
      
        if ("missing" %in% names(row)) {
          if (!is.na(row$missing)) statistics$missing <- row$missing
        }
        if ("N" %in% names(row)) {
          if (!is.na(row$N)) statistics$N <- row$N
        }
        if ("M" %in% names(row)) {
          if (!is.na(row$M)) statistics$M <- row$M
        }
        if ("SD" %in% names(row)) {
          if (!is.na(row$SD)) statistics$SD <- row$SD
        }
        if ("SE" %in% names(row)) {
          if (!is.na(row$SE)) statistics$SE <- row$SE
        }
        if ("min" %in% names(row)) {
          if (!is.na(row$min)) statistics$min <- row$min
        }
        if ("max" %in% names(row)) {
          if (!is.na(row$max)) statistics$max <- row$max
        }
        if ("range" %in% names(row)) {
          if (!is.na(row$range)) statistics$range <- row$range
        }
        if ("median" %in% names(row)) {
          if (!is.na(row$median)) statistics$median <- row$median
        }
        if ("mode" %in% names(row)) {
          if (!is.na(row$mode)) statistics$mode <- row$mode
        }
        if ("skew" %in% names(row)) {
          if (!is.na(row$skew)) statistics$skew <- row$skew
        }
        if ("kurtosis" %in% names(row)) {
          if (!is.na(row$kurtosis)) statistics$kurtosis <- row$kurtosis
        }
        
        # Add the statistics to the variable's statistics property
        group$statistics <- statistics    
        
        # Add the group to the groups list
        groups[[j]] <- group
      }
      
      # Add the groups list to the variable list
      variable$groups <- groups
      
    } else {
      # Extract statistics
      statistics <- list()
    
      if ("missing" %in% names(subset)) {
          if (!is.na(subset$missing)) statistics$missing <- subset$missing
        }
        if ("N" %in% names(subset)) {
          if (!is.na(subset$N)) statistics$N <- subset$N
        }
        if ("M" %in% names(subset)) {
          if (!is.na(subset$M)) statistics$M <- subset$M
        }
        if ("SD" %in% names(subset)) {
          if (!is.na(subset$SD)) statistics$SD <- subset$SD
        }
        if ("SE" %in% names(subset)) {
          if (!is.na(subset$SE)) statistics$SE <- subset$SE
        }
        if ("min" %in% names(subset)) {
          if (!is.na(subset$min)) statistics$min <- subset$min
        }
        if ("max" %in% names(subset)) {
          if (!is.na(subset$max)) statistics$max <- subset$max
        }
        if ("range" %in% names(subset)) {
          if (!is.na(subset$range)) statistics$range <- subset$range
        }
        if ("median" %in% names(subset)) {
          if (!is.na(subset$median)) statistics$median <- subset$median
        }
        if ("mode" %in% names(subset)) {
          if (!is.na(subset$mode)) statistics$mode <- subset$mode
        }
        if ("skew" %in% names(subset)) {
          if (!is.na(subset$skew)) statistics$skew <- subset$skew
        }
        if ("kurtosis" %in% names(subset)) {
          if (!is.na(subset$kurtosis)) statistics$kurtosis <- subset$kurtosis
        }
      
      # Add the statistics to the variable's statistics property
      variable$statistics <- statistics
    }
    
    # Add variable to the list of variables
    variables[[i]] <- variable
  }
  
  # Add variables to the output
  output$variables <- variables
  
  # Add package information
  package <- list()

  package$name <- "tidystats"
  package$version <- getNamespaceVersion("tidystats")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'tidystats_counts'
#' @export
tidy_stats.tidystats_counts <- function(x, args = NULL) {

  output <- list()
  
  # Add method
  output$method <- "Counts"
  
  # Extract grouping variables
  group_names <- names(x)[!names(x) %in% c("n", "pct")]
  
  # Check if there are any groups, if so, combine the grouping variables into
  # a single column and loop over each group to extract the statistics
  if (length(group_names) != 0) {
    output$name <- paste(group_names, collapse = " - ")
    
    x <- tidyr::unite(x, col = "group", dplyr::all_of(group_names), sep = " - ")
    
    # Create an empty groups list
    groups <- list()
    
    # Loop over each row in the data frame and extract the statistics
    for (i in 1:nrow(x)) {
      # Create an empty group list
      group <- list()
      
      # Select the current row
      row <- x[i, ]
      
      # Set the group name
      group$name <- row$group
        
      # Extract statistics
      statistics <- list()
      
      if ("n" %in% names(row)) statistics$n <- row$n
      if ("pct" %in% names(row)) statistics$pct <- row$pct
      
      # Add the statistics to the variable's statistics property
      group$statistics <- statistics    
        
      # Add the group to the groups list
      groups[[i]] <- group
      
      # Add the groups list to the variable list
      output$groups <- groups
    }
  } else {
    # Extract statistics
    statistics <- list()
    
    if ("n" %in% names(x)) statistics$n <- x$n
    if ("pct" %in% names(x)) statistics$pct <- x$pct
    
    # Add the statistics to the variable's statistics property
    output$statistics <- statistics 
  }
  
  # Add package information
  package <- list()

  package$name <- "tidystats"
  package$version <- getNamespaceVersion("tidystats")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'lmerMod'
#' @export
tidy_stats.lmerMod <- function(x, args = NULL) {
  
  output <- list()
  
  # Get summary statistics
  summary <- summary(x)
  
  # Extract method
  output$method <- "Linear mixed model"
  
  # Add model fit statistics, if we have them
  if ("AIC" %in% names(summary$AICtab)) {
    statistics <- list()
    
    statistics$AIC <- summary$AICtab[[1]]
    statistics$BIC <- summary$AICtab[[2]]
    statistics$log_likelihood <- summary$AICtab[[3]]
    statistics$deviance <- summary$AICtab[[4]]
    statistics$df <- summary$AICtab[[5]]
    
    output$statistics <- statistics
  }
  
  # We create the following nested structure:
  # effects:
  # - random_effects:
  #   - groups:
  #     - terms: for variances
  #     - pairs: for correlations between random effects
  # - fixed_effects:
  #     - terms: for coefficient statistics
  #     - pairs: for correlations between fixed effects
  
  effects <- list()
  
  # Extract random effects
  random_effects <- list()
  
  # Set N to number of observations
  random_effects$statistics$N <- summary[[3]]$dims[[1]]
  
  # Get variance-covariance matrix
  varcor <- summary$varcor
  
  # Loop over each group and set the variance statistics and correlations
  groups <- list()

  for (i in 1:length(varcor)) {
    group <- list()
    
    group$name <- names(varcor)[i]
  
    # Set N for the group, if there is an N  
    if (group$name %in% names(summary$ngrps)) {
      group$statistics$N <- summary$ngrps[names(summary$ngrps) == group$name][[1]]
    }
    
    random_statistics <- varcor[[i]]
    
    # Extract standard deviations
    vars <- attr(random_statistics, "stddev")^2
    SDs <- attr(random_statistics, "stddev")
    
    # Extract variances of each term in a group
    terms <- list()
    
    for (j in 1:length(vars)) {
      term <- list()
      term$name <- names(vars)[j]
      
      # Extract statistics
      statistics <- list()

      statistics$var <- vars[[j]]
      statistics$SD <- SDs[[j]] 

      term$statistics <- statistics
      
      terms[[j]] <- term
    }
      
    # Add terms to group
    group$terms <- terms
    
    # Extract correlation pairs, if there are any
    random_cors <- attr(random_statistics, "correlation")  
    
    if (length(random_cors) > 1) {
      
      # Tidy the matrix
      random_cors <- tidy_matrix(random_cors)
      
      pairs <- list()
      
      for (k in 1:nrow(random_cors)) {
        pair <- list()
        
        names <- list()
        names[[1]] <- random_cors$name1[k]
        names[[2]] <- random_cors$name2[k]
        
        pair$names <- names
        pair$statistics$r <- random_cors$value[k]
        
        pairs[[k]] <- pair
      }
      
      group$pairs <- pairs
    }
    
    # Add group to random
    groups[[i]] <- group
  }
  
  # Add residual statistics to the group
  statistics <- list()
  statistics$var <- attr(varcor, "sc")^2
  statistics$SD <- attr(varcor, "sc")
  
  group <- list()
  group$name <- "Residual"
  group$statistics <- statistics
  
  groups[[i + 1]] <- group
  
  # Add groups to random_effects
  random_effects$groups <- groups
  
  # Add random_effects to effects
  effects$random_effects <- random_effects
  
  # Extract fixed effects
  fixed_effects <- list()
  
  # Get coefficient statistics, which we call terms
  terms <- list()
  coefficients <- stats::coef(summary)

  # Loop over the terms
  for (i in 1:nrow(coefficients)) {
    
    term <- list()
    
    # Add the name of the coefficient
    name <- rownames(coefficients)[i]
    term$name <- name
    
    # Create a new statistics list and add the fixed effect's statistics
    statistics <- list()
    
    estimate <- list()
    estimate$name <- "b"
    estimate$value <- coefficients[i, "Estimate"]
    statistics$estimate <- estimate

    statistics$SE <- coefficients[i, "Std. Error"]
    
    statistic <- list()
    statistic$name <- "t"
    statistic$value <- coefficients[i, "t value"]
    statistics$statistic <- statistic
    
    term$statistics <- statistics
    
    # Add the term to the coefficients list
    terms[[i]] <- term
  }
  
  effects$fixed_effects$terms <- terms
  
  # Extract fixed correlations
  fixed_cors <- attr(summary$vcov, "factors")$correlation
  
  if (length(fixed_cors) > 1) {
    
    # Tidy the matrix
    fixed_cors <- tidy_matrix(fixed_cors)
    
    pairs <- list()
    
    for (i in 1:nrow(fixed_cors)) {
      pair <- list()
      names <- list()
      names[[1]] <- fixed_cors$name1[i]
      names[[2]] <- fixed_cors$name2[i]
      value <- fixed_cors$value[i]
      
      pair$names <- names
      pair$statistics$r <- value
      
      pairs[[i]] <- pair
    }
    
    effects$fixed_effects$pairs <- pairs
  }
  
  # Add effects to output
  output$effects <- effects
  
    # Extract REML criterion at convergence
  if ("REML" %in% names(summary$AICtab)) {
    output$REML_criterion_at_convergence <- summary$AICtab[[1]]
  }
  
  # Add additional convergence information
  output$convergence_code = summary$optinfo$conv$opt
  output$convergence_message = summary$optinfo$conv$lme4$messages
  
  # Add package information
  package <- list()

  package$name <- "lme4"
  package$version <- getNamespaceVersion("lme4")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'lmerModLmerTest'
#' @export
tidy_stats.lmerModLmerTest <- function(x, args = NULL) {
  
  output <- list()
  
  # Get summary statistics
  summary <- summary(x)
  
  # Extract method
  output$method <- "Linear mixed model"
  
  # Add model fit statistics, if we have them
  if ("AIC" %in% names(summary$AICtab)) {
    statistics <- list()
    
    statistics$AIC <- summary$AICtab[[1]]
    statistics$BIC <- summary$AICtab[[2]]
    statistics$log_likelihood <- summary$AICtab[[3]]
    statistics$deviance <- summary$AICtab[[4]]
    statistics$df <- summary$AICtab[[5]]
    
    output$statistics <- statistics
  }
  
  # We create the following nested structure:
  # effects:
  # - random_effects:
  #   - groups:
  #     - terms: for variances
  #     - pairs: for correlations between random effects
  # - fixed_effects:
  #     - terms: for coefficient statistics
  #     - pairs: for correlations between fixed effects
  
  effects <- list()
  
  # Extract random effects
  random_effects <- list()
  
  # Set N to number of observations
  random_effects$statistics$N <- summary[[3]]$dims[[1]]
  
  # Get variance-covariance matrix
  varcor <- summary$varcor
  
  # Loop over each group and set the variance statistics and correlations
  groups <- list()

  for (i in 1:length(varcor)) {
    group <- list()
    
    group$name <- names(varcor)[i]
  
    # Set N for the group, if there is an N  
    if (group$name %in% names(summary$ngrps)) {
      group$statistics$N <- summary$ngrps[names(summary$ngrps) == group$name][[1]]
    }
    
    random_statistics <- varcor[[i]]
    
    # Extract standard deviations
    vars <- attr(random_statistics, "stddev")^2
    SDs <- attr(random_statistics, "stddev")
    
    # Extract variances of each term in a group
    terms <- list()
    
    for (j in 1:length(vars)) {
      term <- list()
      term$name <- names(vars)[j]
      
      # Extract statistics
      statistics <- list()

      statistics$var <- vars[[j]]
      statistics$SD <- SDs[[j]] 

      term$statistics <- statistics
      
      terms[[j]] <- term
    }
      
    # Add terms to group
    group$terms <- terms
    
    # Extract correlation pairs, if there are any
    random_cors <- attr(random_statistics, "correlation")  
    
    if (length(random_cors) > 1) {
      
      # Tidy the matrix
      random_cors <- tidy_matrix(random_cors)
      
      pairs <- list()
      
      for (k in 1:nrow(random_cors)) {
        pair <- list()
        
        names <- list()
        names[[1]] <- random_cors$name1[k]
        names[[2]] <- random_cors$name2[k]
        
        pair$names <- names
        pair$statistics$r <- random_cors$value[k]
        
        pairs[[k]] <- pair
      }
      
      group$pairs <- pairs
    }
    
    # Add group to random
    groups[[i]] <- group
  }
  
  # Add residual statistics to the group
  statistics <- list()
  statistics$var <- attr(varcor, "sc")^2
  statistics$SD <- attr(varcor, "sc")
  
  group <- list()
  group$name <- "Residual"
  group$statistics <- statistics
  
  groups[[i + 1]] <- group
  
  # Add groups to random
  random_effects$groups <- groups
  
  # Add random_effects to effects
  effects$random_effects <- random_effects
  
  # Extract fixed effects
  fixed_effects <- list()
  
  # Get coefficient statistics, which we call terms
  terms <- list()
  coefficients <- stats::coef(summary)

  # Loop over the terms
  for (i in 1:nrow(coefficients)) {
    
    term <- list()
    
    # Add the name of the coefficient
    name <- rownames(coefficients)[i]
    term$name <- name
    
    # Create a new statistics list and add the fixed effect's statistics
    statistics <- list()
    
    estimate <- list()
    estimate$name <- "b"
    estimate$value <- coefficients[i, "Estimate"]
    statistics$estimate <- estimate
    
    statistics$SE <- coefficients[i, "Std. Error"]
    statistics$df <- coefficients[i, "df"]
    
    statistic <- list()
    statistic$name <- "t"
    statistic$value <- coefficients[i, "t value"]
    statistics$statistic <- statistic
    
    statistics$p <- coefficients[i, "Pr(>|t|)"]
    
    term$statistics <- statistics
    
    # Add the term to the coefficients list
    terms[[i]] <- term
  }
  
  effects$fixed_effects$terms <- terms
  
  # Extract fixed correlations
  fixed_cors <- attr(summary$vcov, "factors")$correlation
  
  if (length(fixed_cors) > 1) {
    
    # Tidy the matrix
    fixed_cors <- tidy_matrix(fixed_cors)
    
    pairs <- list()
    
    for (i in 1:nrow(fixed_cors)) {
      pair <- list()
      names <- list()
      names[[1]] <- fixed_cors$name1[i]
      names[[2]] <- fixed_cors$name2[i]
      value <- fixed_cors$value[i]
      
      pair$names <- names
      pair$statistics$r <- value
      
      pairs[[i]] <- pair
    }
    
    effects$fixed_effects$pairs <- pairs
  }
  
  # Add effects to output
  output$effects <- effects
  
   # Extract REML criterion at convergence
  output$REML_criterion_at_convergence <- summary$AICtab[[1]]
  
  # Add additional convergence information
  output$convergence_code = summary$optinfo$conv$opt
  output$convergence_message = summary$optinfo$conv$lme4$messages
  
  # Add package information
  package <- list()

  package$name <- "lme4"
  package$version <- getNamespaceVersion("lme4")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'BayesFactor'
#' @export
tidy_stats.BFBayesFactor <- function(x, args = NULL) {
  
  output <- list()
  
  # Determine the method
  class <- class(x@numerator[[1]])[1]
  output$method <- dplyr::case_when(
    class == "BFoneSample" ~ "Bayesian t-test",
    class == "BFlinearModel" ~ "Bayesian linear regression",
    class == "BFcorrelation" ~ "Bayesian correlation",
    class == "BFcontingencyTable" ~ "Bayesian contingency table",
    class == "BFproportion" ~ "Bayesian analysis of proportions",
    class == "BFmetat" ~ "Bayesian meta-analysis"
  )
  
  # Extract bayes factors
  bayes_factors <- BayesFactor::extractBF(x)
  
  # Check whether the test contains one or more models
  if (nrow(bayes_factors) > 1) {
    # Create an empty models list
    models <- list()
    
    for (i in 1:nrow(bayes_factors)) {
      # Create a model and statistics list
      model <- list()
      statistics <- list()
      
      # Extract name of the model
      model$name <- rownames(bayes_factors)[i]
      
      # Extract statistics
      statistics$BF_01 <- bayes_factors$bf[i]
      statistics$BF_10 <- 1/bayes_factors$bf[i]
      statistics$error <- bayes_factors$error[i]
      
      # Add statistics to model and model to models
      model$statistics <- statistics
      models[[i]] <- model
    }
    
    # Add models to output
    output$models <- models
  } else {
    # Extract the name of the model
    output$name <- rownames(bayes_factors)[1]
    
    # Create a statistics list
    statistics <- list()
    
    # Extract statistics
    statistics$BF_01 <- bayes_factors$bf[1]
    statistics$BF_10 <- 1/bayes_factors$bf[1]
    statistics$error <- bayes_factors$error[1]
    
    # Add statistics to output
    output$statistics <- statistics
  }
  
  # Add denominator model information
  output$alternative$name <- x@denominator@longName
  output$alternative$formula <- x@denominator@identifier$formula
  
  # Add package information
  package <- list()

  package$name <- "BayesFactor"
  package$version <- getNamespaceVersion("BayesFactor")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'afex_aov'
#' @export
tidy_stats.afex_aov <- function(x, args = NULL) {

  output <- list()
  
  # Set method
  output$method <- "ANOVA"
  
  # Set the DV
  output$DV <- attr(x, "dv")
  
  # Create an empty terms list
  terms <- list()
  
  # Convert the results to a data frame
  df <- tibble::as_tibble(x$anova_table, rownames = "effect")
  
  for (i in 1:nrow(df)) {
    
    # Create a new term list
    term <- list()
    
    # Add the name of the term
    term$name <- df$effect[i]
    
    # Create a new statistics list and add the coefficient's statistics
    statistics <- list()
    
    statistics$MS <- df$MSE[i]
    statistics$statistic <- list(
      name = "F", 
      value = df$`F`[i]
    )
    statistics$dfs <- list(
      df_numerator = df$`num Df`[i], 
      df_denominator = df$`den Df`[i]
    )
    statistics$p <- df$`Pr(>F)`[i]
    statistics$ges <- df$ges[i]
    
    term$statistics <- statistics
    
    # Add the term data to the terms list
    terms[[i]] <- term
  }
  
  # Add terms to the output
  output$terms <- terms
  
  # Add additional information
  output$type <- attr(x, "type")
  output$sphericity_correction_method <- attr(x$anova_table, "correction")
  
  # Add package information
  package <- list()

  package$name <- "afex"
  package$version <- getNamespaceVersion("afex")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}


#' @describeIn tidy_stats tidy_stats method for class 'emmGrid'
#' @export
tidy_stats.emmGrid <- function(x, args = NULL) {
  output <- list()

  # Convert object to a data frame
  df <- as.data.frame(x)
  
  # Set method
  type <- x@misc$estType
  
  if (type == "contrast" | type == "pairs") {
    output$method <- "contrast"  
  } else {
    output$method <- "EMM"
  }
  
  # Determine type of analysis
  by <- x@misc$by.vars
  
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
          term$name <- paste(x@misc$pri.vars, "=", df_group[, x@misc$pri.vars][j])
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

#' @describeIn tidy_stats tidy_stats method for class 'emm_list'
#' @export
tidy_stats.emm_list <- function(x, args = NULL) {
  stop(paste("You're trying to tidy an object of class 'emm_list'; ", 
    "please provide an object with class 'emmGrid'."))
}

#' @describeIn tidy_stats tidy_stats method for class 'icclist'
#' @export
tidy_stats.icclist <- function(x, args = NULL) {
  output <- list()

  # Set method
  output$method <- "ICC" 
  
  # Set additional ICC information
  output$model <- x$model
  output$type <- x$type
  output$unit <- x$unit
  output$ICC_name <- x$icc.name
  
  # Extract statistics
  statistics <- list()
  
  statistics$N_subjects <- x$subjects
  statistics$N_raters <- x$raters
  statistics$ICC <- x$value
  statistics$statistic$name <- "F"
  statistics$statistic$value <- x$Fvalue
  statistics$dfs$df_numerator <- x$df1
  statistics$dfs$df_denominator <- x$df2
  statistics$p <- x$p.value
  statistics$CI$CI_level <- x$conf.level
  statistics$CI$CI_lower <- x$lbound
  statistics$CI$CI_upper <- x$ubound
  
  # Add statistics to output
  output$statistics <- statistics
  
  # Add additional information
  output$null_value <- x$r0
  
  # Add package information
  package <- list()

  package$name <- "irr"
  package$version <- getNamespaceVersion("irr")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'effsize'
#' @export
tidy_stats.effsize <- function(x, args = NULL) {
  output <- list()
  
  # Set method
  output$method <- x$method
  
  # Extract statistics
  statistics <- list()

  statistics$statistic$name <- x$name
  statistics$statistic$value <- x$estimate
  statistics$CI$CI_level <- x$conf.level
  statistics$CI$CI_lower <- x$conf.int["lower"]
  statistics$CI$CI_upper <- x$conf.int["upper"]
  statistics$var <- x$var
  statistics$SD <- x$sd
  
  # Add statistics to output
  output$statistics <- statistics
  
  # Add additional information
  output$magnitude <- x$magnitude
  
  # Add package information
  package <- list()

  package$name <- "effsize"
  package$version <- getNamespaceVersion("effsize")[[1]]
  
  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'lavaan'
#' @export
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
  
  # Latent variables
  # Create an empty list for the latent variables
  latent_variables <- list()
  
  # Select only the latent variables statistics from the PE data
  PE_latent <- dplyr::filter(PE, op == "=~")
  
  # Loop
  for (i in 1:nrow(PE_latent)) {
    var <- PE_latent[i, ]
    
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
    
    latent_variables[[i]] <- latent_variable
  }
  
  # Add latent variables to output
  output$latent_variables <- latent_variables
  
  # Regressions
  # Create an empty list for the regressions
  regressions <- list()
  
  # Select only the regressions statistics from the PE data
  PE_regressions <- dplyr::filter(PE, op == "~")
  
  # Loop, if there are any regressions
  if (nrow(PE_regressions) > 0) {
    for (i in 1:nrow(PE_regressions)) {
      reg <- PE_regressions[i, ]
      
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
      
      regressions[[i]] <- regression
    }
    
    # Add regressions to output
    output$regressions <- regressions
  }
  
  # Covariances
  # Create an empty list for the covariances
  covariances <- list()
  
  # Select only the covariance statistics from the PE data
  PE_covariances <- dplyr::filter(PE, op == "~~" & lhs != rhs)
  
  # Loop
  for (i in 1:nrow(PE_covariances)) {
    covar <- PE_covariances[i, ]
    
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
        covariance$statistics$std_lv <- var$std.lv
        covariance$statistics$std_all <- var$std.all
        covariance$statistics$std_nox <- var$std.nox
      }  
    }
    
    if (!is.null(args$ci)) {
      if (args$ci) {
        covariance$statistics$CI$CI_level <- .90
        covariance$statistics$CI$CI_lower <- var$ci.lower
        covariance$statistics$CI$CI_upper <- var$ci.upper
      }
    }
    
    covariances[[i]] <- covariance
  }
  
  # Variances
  # Create an empty list for the variances
  variances <- list()
  
  # Select only the variance statistics from the PE data
  PE_variances <- dplyr::filter(PE, lhs == rhs)
  
  # Loop
  for (i in 1:nrow(PE_variances)) {
    var <- PE_variances[i, ]
    
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
    
    variances[[i]] <- variance
  }

  # Add covariances and variances to output  
  output$covariances <- covariances
  output$variances <- variances
  
  # Add package information
  package <- list()

  package$name <- "lavaan"
  package$version <- getNamespaceVersion("lavaan")[[1]]
  
  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'psych'
#' @export
tidy_stats.psych <- function(x, args = NULL) {
  output <- list()
  
  # Check the kind of psych object
  if ("alpha" %in% class(x)) {
    output$method <- "Reliability"
    
    # Extract statistics
    statistics <- list()
    
    statistics$alpha <- x$total$raw_alpha
    statistics$alpha_z <- x$total$std.alpha
    statistics$G6 <- x$total$`G6(smc)`
    statistics$r_mean <- x$total$average_r
    statistics$r_median <- x$total$median_r
    statistics$SNR <- x$total$`S/N`
    if ("mean" %in% names(x$total)) {
      statistics$mean <- x$total$mean
      statistics$sd <- x$total$sd
    }
    
    # Add statistics to the output
    output$statistics <- statistics
    
  } else if ("corr.test" %in% class(x)) {
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
  
  # Add package information
  package <- list()

  package$name <- "psych"
  package$version <- getNamespaceVersion("psych")[[1]]
  
  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'confint'
#' @export
tidy_stats.confint <- function(x, args = NULL) {
  output <- list()
  
  # Set method
  output$method <- "Confidence intervals"
  
  # Extract confidence level
  CI_bounds <- readr::parse_number(colnames(x))
  CI_level <- (CI_bounds[2] - CI_bounds[1]) / 100
  
  # Check if there is 1 or more terms
  # If 1, only create a statistics list
  # If multiple, loop over terms and create separate lists for each term
  if (length(rownames(x)) == 1) {
    output$name <- rownames(x)[1]
    
    output$statistics$CI$CI_level <- CI_level
    output$statistics$CI$CI_lower <- x[1]
    output$statistics$CI$CI_upper <- x[2]
  } else {
    terms <- list()
    
    for (i in 1:length(rownames(x))) {
      term <- list()
       
      
      term$name <- rownames(x)[i]
      term$statistics$CI$CI_level <- CI_level
      term$statistics$CI$CI_lower <- x[i]
      term$statistics$CI$CI_upper <- x[i + length(rownames(x))]
      
      terms[[i]] <- term
    }
    
    output$terms <- terms 
  }
  
  # Add package information
  package <- list()

  package$name <- "stats"
  package$version <- getNamespaceVersion("stats")[[1]]
  
  # Add package information to output
  output$package <- package
  
  return(output)
}
