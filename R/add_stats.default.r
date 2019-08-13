#' add_stats default function
#' 
#' \code{add_stats.default} is the default add_stats() function. It takes the
#' output of a statistical test, organizes the output, and adds it to a 
#' tidystats list. While adding the output, additional information about the 
#' test can be supplied, including the type of test (primary, secondary, or 
#' exploratory), whether the test was preregistered, and additional notes.
#' 
#' Please note that not all statistical tests are currently supported. See 
#' 'Details' below for a list of supported statistical tests.
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
#' - t.test()
#' - cor.test()
#' - chisq.test()
#' - wilcox.test()
#' - fisher.test()
#' - oneway.test()
#' - aov()
#' - lm()
#' 
#' @export

add_stats.default <- function(results, output, identifier = NULL, type = NULL,
  preregistered = NULL, notes = NULL, class = NULL, args = NULL) {

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
