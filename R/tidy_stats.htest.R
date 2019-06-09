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

    output$hybridPars <- hybridPars
  } else if (stringr::str_detect(model$method, "Two Sample t-test")) {
    # (use trimws to remove the leading space from a Two Sample t-test)
    output$method <- trimws(model$method)
  } else {
    output$method <- model$method
  }

  # Extract statistics
  statistics <- list()

  if ("statistic" %in% names(model)) {
    statistics[names(model$statistic)] <- model$statistic
  }
  if ("parameter" %in% names(model)) {
    statistics[names(model$parameter)] <- model$parameter
  }
  if ("p.value" %in% names(model)) {
    statistics$p.value <- model$p.value
  }
  if ("estimate" %in% names(model)) {
    statistics[names(model$estimate)] <- model$estimate
  }
  if ("cor" %in% names(model)) {
    statistics[names(model$cor)] <- model$cor
  }
  if ("stderr" %in% names(model)) {
    statistics$stderr <- model$stderr
  }

  # Extract confidence intervals
  if ("conf.int" %in% names(model)) {
    CIs <- list()

    CIs$level <- attr(model$conf.int, "conf.level")
    CIs$lower <- model$conf.int[1]
    CIs$upper <- model$conf.int[2]

    statistics$CI <- CIs
  }

  # Add statistics to output
  output$statistics <- statistics

  # Add alternative hypothesis information
  if ("alternative" %in% names(model)) {
    alternative <- list()

    alternative$direction <- model$alternative
    alternative[names(model$null.value)] <- model$null.value

    # Add alternative hypothesis information to output
    output$alternative <- alternative
  }

  # Add data information
  output$data.name <- model$data.name

  # Add package information
  package <- list()

  package$name <- "stats"
  package$version <- getNamespaceVersion("stats")[[1]]

  # Add package information to output
  output$package <- package

  return(output)
}
