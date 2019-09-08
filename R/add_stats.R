#' Add statistical output to a tidystats list
#'
#' \code{add_stats} is used to add the output of a statistical test to a 
#' tidystats list. While adding the output, additional information about the 
#' test can be added, including the type of test (primary, secondary, or 
#' exploratory), whether the test was preregistered, and additional notes. 
#' Please note that not all statistical tests are supported. See 'Details' below
#' for a list of supported statistical tests.
#'
#' @param results A tidystats list.
#' @param output Output of a statistical test.
#' @param identifier A character string identifying the model. Automatically
#' created if not provided.
#' @param type A character string specifying the type of analysis: primary, 
#' secondary, or exploratory.
#' @param preregistered A boolean specifying whether the analysis was 
#' preregistered or not.
#' @param notes A character string specifying additional information.
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
#' # Load dplyr for access to the piping operator
#' library(dplyr)
#' 
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
#' #' # Create an empty list
#' results <- list()
#' 
#' # Add output to the results list
#' results <- results %>%
#'   add_stats(sleep_test) %>%
#'   add_stats(lm_D9, type = "primary", preregistered = TRUE) %>%
#'   add_stats(npk_aov, notes = "An ANOVA example")
#' 
#' @export
add_stats <- function(results, output, identifier = NULL, type = NULL, 
  preregistered = NULL, notes = NULL) UseMethod("add_stats", output)

#' @export
add_stats.default <- function(results, output, identifier = NULL, type = NULL,
  preregistered = NULL, notes = NULL) {

  # Create an identifier if it is not specified, else check whether it already
  # exists
  if (is.null(identifier)) {
    if (deparse(substitute(output)) == ".") {
      identifier <- paste0("M", formatC(length(results) + 1, width = "1",
        format = "d"))
    } else {
      identifier <- deparse(substitute(output))
    }
  } else {
    if (!is.null(names(results))) {
      if (identifier %in% names(results)) {
        stop("Identifier already exists.")
      }
    }
  }

  # Tidy the output
  analysis <- tidy_stats(output)

  # Add type: primary, secondary, or exploratory
  if (!missing(type)) {
    if (type == "primary") {
      analysis$type <- "primary"  
    } else if (type == "secondary") {
      analysis$type <- "secondary"  
    } else if (type == "exploratory") {
      analysis$type <- "exploratory"  
    } else {
      warning(paste("Unknown type; type should be either 'primary',",
        "'secondary', or 'exploratory'."))
    }
  }
  
  # Add whether the analysis was pregistered or not
  if (!missing(preregistered)) {
    if (preregistered) {
      analysis$preregistered <- "yes"
    } else {
      analysis$preregistered <- "no"
    }
  }
  
  # Add notes
  if (!missing(notes)) {
    analysis$notes <- notes
  }

  # Add the new analysis to the list
  results[[identifier]] <- analysis

  # Return the new results list
  return(results)
}
