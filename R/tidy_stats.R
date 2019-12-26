#' Tidy the output of a statistics object
#' 
#' \code{tidy_stats} is used to convert the output of a statistical object to a
#' list of organized statistics. This output can then be added to a list using
#' the \code{add_stats} function of this package. The \code{tidy_stats} function 
#' is automatically run when \code{add_stats} is used, so there is generally no 
#' need to use this function explicitly. It can be used, however, to quickly 
#' peek at how the output of a specific analysis will be organized.Please note 
#' that not all statistical tests are supported. See 'Details' below for a list 
#' of supported statistical tests.
#' 
#' @param x The output of a statistical test.
#' 
#' @details 
#' Currently supported functions:
#' \itemize{
#'   \item \code{t.test()}
#'   \item \code{cor.test()}
#'   \item \code{chisq.test()}
#'   \item \code{wilcox.test()}
#'   \item \code{fisher.test()}
#'   \item \code{oneway.test()}
#'   \item \code{aov()}
#'   \item \code{lm()}
#' }
#' 
#' @examples 
#' # Conduct statistical tests
#' # t-test:
#' sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)
#' 
#' # lm:
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
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
tidy_stats <- function(x) UseMethod("tidy_stats")

#' @describeIn tidy_stats tidy_stats method for class 'htest'
#' @export
tidy_stats.htest <- function(x) {

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
  }

  # Extract DV and IV information
  output$data_name <- x$data.name
  
  # Extract statistics
  statistics <- list()

  # Estimate
  # Special case: Calculate estimate for Two Sample t-tests
    if (length(x$estimate) > 1) {
      statistics$estimate <- x$estimate[[1]] - x$estimate[[2]]
    } else {
      statistics$estimate <- x$estimate[[1]]
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
    dfs$numerator_df <- x$parameter[[1]]
    dfs$denominator_df <- x$parameter[[2]]
    statistics$dfs <- dfs
  } else {
    statistics$df <- x$parameter[[1]]
  }
  
  # p-value
  statistics$p <- x$p.value
  
  # Extract confidence intervals
  if (!is.null(x$conf.int)) {
    CIs <- list()

    CIs$level <- attr(x$conf.int, "conf.level")
    CIs$lower <- x$conf.int[1]
    CIs$upper <- x$conf.int[2]

    statistics$CI <- CIs
  }

  # Add statistics to output
  output$statistics <- statistics
  
  # Add alternative hypothesis information
  if (!is.null(x$alternative)) {
    alternative <- list()

    alternative$direction <- x$alternative
    alternative[names(x$null.value)] <- x$null.value

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
tidy_stats.lm <- function(x) {

  output <- list()
  
  # Get summary statistics
  summary <- summary(x)

  # Extract method
  output$method <- "Linear regression"
  
  # Extract statistics of the coefficients
  coef <- coef(summary)
  
  # Create an empty coefficients list
  coefficients <- list()
  
  for (i in 1:nrow(coef(summary))) {
    
    # Create a new coefficient list
    coefficient <- list()
    
    # Add the name of the coefficient
    name = rownames(coef)[i]
    coefficient$name <- name
    
    # Create a new statistics list and add the coefficient's statistics
    statistics <- list()
    
    statistics$estimate <- coef[name, "Estimate"]
    statistics$SE <- coef[name, "Std. Error"]
    
    statistic <- list()
    statistic$name <- "t"
    statistic$value <- coef[name, "t value"]
    statistics$statistic <- statistic
    
    statistics$df <- summary$df[2]
    
    statistics$p <- coef[name, "Pr(>|t|)"]
    
    coefficient$statistics <- statistics
    
    # Add the coefficient data to the coefficients list
    coefficients[[i]] <- coefficient
  }
  
  # Extract model statistics
  model <- list()
  statistics <- list()
  
  statistics$r_squared <- summary$r.squared
  statistics$adjusted_r_squared <- summary$adj.r.squared
  
  statistic <- list()
  statistic$name <- "F"
  statistic$value <- summary$fstatistic[[1]]
  statistics$statistic <- statistic
  
  dfs <- list()
  dfs$numerator_df <- summary$fstatistic[[2]]
  dfs$denominator_df <- summary$fstatistic[[3]]
  statistics$dfs <- dfs
  
  statistics$p <- stats::pf(summary$fstatistic[[1]], summary$fstatistic[[2]], 
    summary$fstatistic[[3]], lower.tail = FALSE)
  statistics$sigma <- summary$sigma
  
  model$statistics <- statistics
  
  # Add coefficients and model to the output
  output$coefficients <- coefficients
  output$model <- model
  
  # Add package information
  package <- list()

  package$name <- "stats"
  package$version <- getNamespaceVersion("stats")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'lmerMod'
#' @export
tidy_stats.lmerMod <- function(x) {
  
  output <- list()
  
  # Get summary statistics
  summary <- summary(x)
  
  # Extract method
  output$method <- "Linear mixed model"
  
  # Extract REML criterion at convergence
  output$REML_criterion_at_convergence <- summary$AICtab
  
  # Add additional convergence information
  output$convergence_code = summary$optinfo$conv$opt
  output$convergence_message = summary$optinfo$conv$lme4$messages
  
  # Extract statistics into four categories:
  # - Variance of each random effect
  # - Correlations between random effects
  # - Coefficients of each fixed effect
  # - Correlations between fixed effects
  
  # Extract random effects
  random <- list()
  
  # Set N to number of observations
  random$N <- summary[[3]]$dims[1]
  
  # Get variance-covariance matrix
  varcor <- summary$varcor
  
  # Loop over each group and set the variance statistics and correlations
  groups <- list()

  for (i in 1:length(varcor)) {
    group <- list()
    
    group$name <- names(varcor)[i]
  
    # Set N for the group, if there is an N  
    if (group$name %in% names(summary$ngrps)) {
      group$N <- summary$ngrps[names(summary$ngrps) == group$name]
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

      statistics$var <- vars[j] 
      statistics$SD <- SDs[j] 

      term$statistics <- statistics
      
      terms[[j]] <- term
    }
      
    # Add terms to group
    group$variances <- terms
    
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
        pair$r <- random_cors$value[k]
        
        pairs[[k]] <- pair
      }
      
      group$correlations <- pairs
    }
    
    # Add group to random
    groups[[i]] <- group
  }
  
  # Add residual statistics to the group
  statistics <- list()
  statistics$var <- attr(random_effects, "sc")^2
  statistics$SD <- attr(random_effects, "sc")
  
  group <- list()
  group$name <- "Residual"
  group$variances[[1]]$statistics <- statistics
  
  groups[[i + 1]] <- group
  
  # Add groups to random
  random$groups <- groups
  
  # Add random to output
  output$random_effects <- random
  
  # Extract fixed effects
  fixed <- list()
  
  # Get coefficient statistics
  coefficients <- list()
  coef <- coef(summary)

  # Loop over the terms
  for (i in 1:nrow(coef)) {
    
    term <- list()
    
    # Add the name of the coefficient
    name <- rownames(coef)[i]
    term$name <- name
    
    # Create a new statistics list and add the fixed effect's statistics
    statistics <- list()
    
    statistics$estimate <- coef[name, "Estimate"]
    statistics$SE <- coef[name, "Std. Error"]
    
    statistic <- list()
    statistic$name <- "t"
    statistic$value <- coef[name, "t value"]
    statistics$statistic <- statistic
    
    term$statistics <- statistics
    
    # Add the term to the coefficients list
    coefficients[[i]] <- term
  }
  
  output$fixed_effects$coefficients <- coefficients
  
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
      pair$r <- value
      
      pairs[[i]] <- pair
    }
    
    output$fixed_effects$correlations <- pairs
  }
  
  # Add package information
  package <- list()

  package$name <- "lme4"
  package$version <- getNamespaceVersion("lme4")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'lmerMod'
#' @export
tidy_stats.lmerModLmerTest <- function(x) {
  
  output <- list()
  
  # Get summary statistics
  summary <- summary(x)
  
  # Extract method
  output$method <- "Linear mixed model"
  
  # Extract REML criterion at convergence
  output$REML_criterion_at_convergence <- summary$AICtab
  
  # Add additional convergence information
  output$convergence_code = summary$optinfo$conv$opt
  output$convergence_message = summary$optinfo$conv$lme4$messages
  
  # Extract statistics into four categories:
  # - Variance of each random effect
  # - Correlations between random effects
  # - Coefficients of each fixed effect
  # - Correlations between fixed effects
  
  # Extract random effects
  random <- list()
  
  # Set N to number of observations
  random$N <- summary[[3]]$dims[1]
  
  # Get variance-covariance matrix
  varcor <- summary$varcor
  
  # Loop over each group and set the variance statistics and correlations
  groups <- list()

  for (i in 1:length(varcor)) {
    group <- list()
    
    group$name <- names(varcor)[i]
  
    # Set N for the group, if there is an N  
    if (group$name %in% names(summary$ngrps)) {
      group$N <- summary$ngrps[names(summary$ngrps) == group$name]
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

      statistics$var <- vars[j] 
      statistics$SD <- SDs[j] 

      term$statistics <- statistics
      
      terms[[j]] <- term
    }
      
    # Add terms to group
    group$variances <- terms
    
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
        pair$r <- random_cors$value[k]
        
        pairs[[k]] <- pair
      }
      
      group$correlations <- pairs
    }
    
    # Add group to random
    groups[[i]] <- group
  }
  
  # Add residual statistics to the group
  statistics <- list()
  statistics$var <- attr(random_effects, "sc")^2
  statistics$SD <- attr(random_effects, "sc")
  
  group <- list()
  group$name <- "Residual"
  group$variances[[1]]$statistics <- statistics
  
  groups[[i + 1]] <- group
  
  # Add groups to random
  random$groups <- groups
  
  # Add random to output
  output$random_effects <- random
  
  # Extract fixed effects
  fixed <- list()
  
  # Get coefficient statistics
  coefficients <- list()
  coef <- coef(summary)

  # Loop over the terms
  for (i in 1:nrow(coef)) {
    
    term <- list()
    
    # Add the name of the coefficient
    name <- rownames(coef)[i]
    term$name <- name
    
    # Create a new statistics list and add the fixed effect's statistics
    statistics <- list()
    
    statistics$estimate <- coef[name, "Estimate"]
    statistics$SE <- coef[name, "Std. Error"]
    statistics$df <- coef[name, "df"]
    
    statistic <- list()
    statistic$name <- "t"
    statistic$value <- coef[name, "t value"]
    statistics$statistic <- statistic
    
    statistics$p <- coef[name, "Pr(>|t|)"]
    
    term$statistics <- statistics
    
    # Add the term to the coefficients list
    coefficients[[i]] <- term
  }
  
  output$fixed_effects$coefficients <- coefficients
  
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
      pair$r <- value
      
      pairs[[i]] <- pair
    }
    
    output$fixed_effects$correlations <- pairs
  }
  
  # Add package information
  package <- list()

  package$name <- "lmerTest"
  package$version <- getNamespaceVersion("lmerTest")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}

#' @describeIn tidy_stats tidy_stats method for class 'aov'
#' @export
tidy_stats.aov <- function(x) {

  output <- list()
  
  # Get summary statistics
  summary <- summary(x)

  # Extract method
  output$method <- "ANOVA"
  
  # Create an empty coefficients list
  coefficients <- list()
  
  # Convert the summary statistics format to a data frame
  summary <- tibble::as_tibble(summary[[1]], rownames = "terms")
  
  # Trim spaces from the names of the terms
  summary <- dplyr::mutate(summary, terms = stringr::str_trim(terms))
  
  for (i in 1:nrow(summary)) {
    
    # Create a new coefficient list
    coefficient <- list()
    
    # Add the name of the coefficient
    name = summary$terms[i]
    coefficient$name <- name
    
    # Create a new statistics list and add the coefficient's statistics
    statistics <- list()
    
    statistics$SS <- summary$`Sum Sq`[i]
    statistics$MS <- summary$`Mean Sq`[i]
    
    if (name != "Residuals") {
      statistic <- list()
      statistic$name <- "F"
      statistic$value <- summary$`F value`[i]
      statistics$statistic <- statistic
    }
    
    statistics$df <- summary$Df[i]
    
    if (name != "Residuals") {
      statistics$p <- summary$`Pr(>F)`[i]
    }
    
    coefficient$statistics <- statistics
    
    # Add the coefficient data to the coefficients list
    coefficients[[i]] <- coefficient
  }
  
  # Add coefficients to the output
  output$coefficients <- coefficients
  
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
tidy_stats.aovlist <- function(x) {

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
    
    # Create an empty coefficients list
    coefficients <- list()
    
    # Convert the summary statistics format to a data frame
    summary_df <- tibble::as_tibble(as.data.frame(as.list.data.frame(
      summary[[i]])), rownames = "terms")
    
    # Trim spaces from the names of the terms
    summary_df <- dplyr::mutate(summary_df, terms = stringr::str_trim(terms))
    
    for (j in 1:nrow(summary_df)) {
    
      # Create a new coefficient list
      coefficient <- list()
      
      # Add the name of the coefficient
      name = summary_df$terms[j]
      coefficient$name <- name
      
      # Create a new statistics list and add the coefficient's statistics
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
    
      coefficient$statistics <- statistics
    
      # Add the coefficient data to the coefficients list
      coefficients[[j]] <- coefficient
    }
    
    # Add coefficients to the group
    group$coefficients <- coefficients
    
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
tidy_stats.tidystats_descriptives <- function(x) {

  output <- list()
  
  # Add method
  output$method <- "Descriptives"
  
  # Extract variable information
  var_name <- dplyr::first(dplyr::pull(x, variable))
  
  # Extract grouping information
  group_names <- dplyr::group_vars(x)
  n_groups <- length(group_names)
  
  # Set the name property
  output$name <- var_name
  
  # Check whether there are any grouping variables and select the relevant
  # row of descriptives
  if (n_groups > 0) {
    
    # Set the grouping name
    # If there is more than 1 grouping variable, combine them together
    output$group_by = paste(group_names, collapse = " by ")
    
    # Create an empty groups list
    groups <- list()
    
    # Loop over the groups
    for (i in 1:nrow(x)) {
      # Create an empty group list
      group <- list()
      
      # Select the current row
      row <- x[i, ]
      
      # Set the group name
      group$name <- paste(unlist(row[group_names]), collapse = " - ")
      
      # Extract statistics
      statistics <- list()
    
      if ("missing" %in% names(row)) statistics$missing <- row$missing
      if ("N" %in% names(row)) statistics$N <- row$N
      if ("M" %in% names(row)) statistics$M <- row$M
      if ("SD" %in% names(row)) statistics$SD <- row$SD
      if ("SE" %in% names(row)) statistics$SE <- row$SE
      if ("min" %in% names(row)) statistics$min <- row$min
      if ("max" %in% names(row)) statistics$max <- row$max
      if ("range" %in% names(row)) statistics$range <- row$range
      if ("median" %in% names(row)) statistics$median <- row$median
      if ("mode" %in% names(row)) statistics$mode <- row$mode
      if ("skew" %in% names(row)) statistics$skew <- row$skew
      if ("kurtosis" %in% names(row)) statistics$kurtosis <- row$kurtosis
      
      # Add the statistics to the variable's statistics property
      group$statistics <- statistics    
      
      # Add the group to the groups list
      groups[[i]] <- group
    }
    
    # Add the groups list to the variable list
    output$groups <- groups
    
  } else {
    # Extract statistics
    statistics <- list()
  
    if ("missing" %in% names(x)) statistics$missing <- x$missing
    if ("N" %in% names(x)) statistics$N <- x$N
    if ("M" %in% names(x)) statistics$M <- x$M
    if ("SD" %in% names(x)) statistics$SD <- x$SD
    if ("SE" %in% names(x)) statistics$SE <- x$SE
    if ("min" %in% names(x)) statistics$min <- x$min
    if ("max" %in% names(x)) statistics$max <- x$max
    if ("range" %in% names(x)) statistics$range <- x$range
    if ("median" %in% names(x)) statistics$median <- x$median
    if ("mode" %in% names(x)) statistics$mode <- x$mode
    if ("skew" %in% names(x)) statistics$skew <- x$skew
    if ("kurtosis" %in% names(x)) statistics$kurtosis <- x$kurtosis
    
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

#' @describeIn tidy_stats tidy_stats method for class 'tidystats_counts'
#' @export
tidy_stats.tidystats_counts <- function(x) {

  output <- list()
  
  # Add method
  output$method <- "Counts"
  
  # Extract groups
  cols <- c("n", "pct")
  
  names(x) %>%
    
  
  group_names <- x %>%
    select(-one_of(cols)) %>%
    names()
  n_groups <- length(group_names)
  
  # Set the name property
  output$name <- var_name
  
  # Check whether there are any grouping variables and select the relevant
  # row of descriptives
  if (n_groups > 0) {
    
    # Set the grouping name
    # If there is more than 1 grouping variable, combine them together
    output$group_by = paste(group_names, collapse = " by ")
    
    # Create an empty groups list
    groups <- list()
    
    # Loop over the groups
    for (i in 1:nrow(x)) {
      # Create an empty group list
      group <- list()
      
      # Select the current row
      row <- x[i, ]
      
      # Set the group name
      group$name <- paste(unlist(row[group_names]), collapse = " - ")
      
      # Extract statistics
      statistics <- list()
    
      if ("missing" %in% names(row)) statistics$missing <- row$missing
      if ("N" %in% names(row)) statistics$N <- row$N
      if ("M" %in% names(row)) statistics$M <- row$M
      if ("SD" %in% names(row)) statistics$SD <- row$SD
      if ("SE" %in% names(row)) statistics$SE <- row$SE
      if ("min" %in% names(row)) statistics$min <- row$min
      if ("max" %in% names(row)) statistics$max <- row$max
      if ("range" %in% names(row)) statistics$range <- row$range
      if ("median" %in% names(row)) statistics$median <- row$median
      if ("mode" %in% names(row)) statistics$mode <- row$mode
      if ("skew" %in% names(row)) statistics$skew <- row$skew
      if ("kurtosis" %in% names(row)) statistics$kurtosis <- row$kurtosis
      
      # Add the statistics to the variable's statistics property
      group$statistics <- statistics    
      
      # Add the group to the groups list
      groups[[i]] <- group
    }
    
    # Add the groups list to the variable list
    output$groups <- groups
    
  } else {
    # Extract statistics
    statistics <- list()
  
    if ("missing" %in% names(x)) statistics$missing <- x$missing
    if ("N" %in% names(x)) statistics$N <- x$N
    if ("M" %in% names(x)) statistics$M <- x$M
    if ("SD" %in% names(x)) statistics$SD <- x$SD
    if ("SE" %in% names(x)) statistics$SE <- x$SE
    if ("min" %in% names(x)) statistics$min <- x$min
    if ("max" %in% names(x)) statistics$max <- x$max
    if ("range" %in% names(x)) statistics$range <- x$range
    if ("median" %in% names(x)) statistics$median <- x$median
    if ("mode" %in% names(x)) statistics$mode <- x$mode
    if ("skew" %in% names(x)) statistics$skew <- x$skew
    if ("kurtosis" %in% names(x)) statistics$kurtosis <- x$kurtosis
    
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

#' @describeIn tidy_stats tidy_stats method for class 'anova'
#' @export
tidy_stats.anova <- function(x) {
  
  output <- list()
  
  # Add method information
  output$method <- "ANOVA"
  
  # Create an empty coefficients list
  coefficients <- list()
  
  # Convert the summary statistics format to a data frame
  x <- tibble::as_tibble(x, rownames = "terms")
  
  # Trim spaces from the names of the terms
  x <- dplyr::mutate(x, terms = stringr::str_trim(terms))
  
  for (i in 1:nrow(x)) {
    
    # Create a new coefficient list
    coefficient <- list()
    
    # Add the name of the coefficient
    name = x$terms[i]
    coefficient$name <- name
    
    # Create a new statistics list and add the coefficient's statistics
    statistics <- list()
    
    statistics$SS <- x$`Sum Sq`[i]
    statistics$MS <- x$`Mean Sq`[i]
    
    if (name != "Residuals") {
      statistic <- list()
      statistic$name <- "F"
      statistic$value <- x$`F value`[i]
      statistics$statistic <- statistic
    }
    
    statistics$df <- x$Df[i]
    
    if (name != "Residuals") {
      statistics$p <- x$`Pr(>F)`[i]
    }
    
    coefficient$statistics <- statistics
    
    # Add the coefficient data to the coefficients list
    coefficients[[i]] <- coefficient
  }
  
  # Add coefficients to the output
  output$coefficients <- coefficients
  
  # Add package information
  package <- list()

  package$name <- "stats"
  package$version <- getNamespaceVersion("stats")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}
