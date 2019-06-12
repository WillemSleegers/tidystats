#' Create a tidy stats json object from an htest object
#'
#' @export
tidy_stats.htest <- function(model) {

  output <- list()

  # Extract method
  # Extract number of simulations from Fisher's test based on simulated p-values
  if (stringr::str_detect(model$method, "simulated p-value")) {
    output$method <- "Fisher's Exact Test for Count Data with simulated p-value"

    output$sim <- as.numeric(stringr::str_extract(model$method,
      "[0-9](e\\+)?([0-9].)?"))
  # Extract parameters from Fisher's test using sym. chisq
  } else if (stringr::str_detect(model$method, "hybrid using asym")) {
    output$method <- paste("Fisher's Exact Test for Count Data",
      "hybrid using asym.chisq")

    hybridPars <- list()

    hybridPars$expect = readr::parse_number(stringr::str_extract(model$method,
      "exp=[0-9+]"))
    hybridPars$percent = readr::parse_number(stringr::str_extract(model$method,
      "perc=[0-9+]"))
    hybridPars$Emin = readr::parse_number(stringr::str_extract(model$method,
      "exp=[0-9+]"))

    output$hybrid_parameters <- hybridPars
  } else if (stringr::str_detect(model$method, "Two Sample t-test")) {
    # (use trimws to remove the leading space from a Two Sample t-test)
    output$method <- trimws(model$method)
    
    if (stringr::str_detect(model$method, "Welch")) {
      output$var_equal <- FALSE
    } else {
      output$var_equal <- TRUE
    }
    
  } else if (stringr::str_detect(model$method, 
    "One-way analysis of means (not assuming equal variances)")) {
    output$method <- "One-way analysis of means"
    output$var_equal <- FALSE
  } else {
    output$method <- model$method
  }

  # Extract statistics
  statistics <- list()

  # Estimate
  # Special case: Calculate estimate for Two Sample t-tests
    if (length(model$estimate) > 1) {
      statistics$estimate <- model$estimate[[1]] - model$estimate[[2]]
    } else {
      statistics$estimate <- model$estimate[[1]]
    }
  
  # SE
  if (!is.null(model$stderr)) {
    statistics$SE <- model$stderr
  }
  
  # Test statistic
  statistic <- list()
  statistic$name <- names(model$statistic)
  statistic$value <- model$statistic[[1]]
  statistics$statistic <- statistic
  
  # Degrees of freedom
  # Special case: One-way analysis of means without equal variance assumption
  if (length(model$parameter) > 1) {
    dfs <- list()
    dfs$numerator_df <- model$parameter[[1]]
    dfs$denominator_df <- model$parameter[[2]]
    statistics$dfs <- dfs
  } else {
    statistics$df <- model$parameter[[1]]
  }
  
  # p-value
  statistics$p <- model$p.value
  
  # Extract confidence intervals
  if (!is.null(model$conf.int)) {
    CIs <- list()

    CIs$level <- attr(model$conf.int, "conf.level")
    CIs$lower <- model$conf.int[1]
    CIs$upper <- model$conf.int[2]

    statistics$CI <- CIs
  }

  # Add statistics to output
  output$statistics <- statistics
  
  # Add alternative hypothesis information
  if (!is.null(model$alternative)) {
    alternative <- list()

    alternative$direction <- model$alternative
    alternative[names(model$null.value)] <- model$null.value

    # Add alternative hypothesis information to output
    output$alternative <- alternative
  }
  
  # Add data information
  output$data_name <- model$data.name

  # Add package information
  package <- list()

  package$name <- "stats"
  package$version <- getNamespaceVersion("stats")[[1]]

  # Add package information to output
  output$package <- package

  return(output)
}
