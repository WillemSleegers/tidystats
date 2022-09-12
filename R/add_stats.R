#' Add statistical output to a tidystats list
#'
#' \code{add_stats} is used to add the output of a statistical test to a 
#' tidystats list. While adding the output, additional information about the 
#' test can be added, including the type of test (primary, secondary, or 
#' exploratory), whether the test was preregistered, and additional notes. 
#' Please note that not all statistical tests are supported. See 'Details' below
#' for a list of supported statistical tests.
#'
#' @param list A tidystats list.
#' @param output Output of a statistical test.
#' @param identifier A character string identifying the model. Automatically
#' created if not provided.
#' @param type A character string specifying the type of analysis: primary, 
#' secondary, or exploratory.
#' @param preregistered A boolean specifying whether the analysis was 
#' preregistered or not.
#' @param notes A character string specifying additional information.
#' @param args A list of additional arguments to customize which statistics 
#' should be extracted. See details for a list of supported analyses.
#' @param class A character string to manually specify the class of the object
#' so that tidystats knows how to extract the statistics. See details for a list 
#' classes that are supported.
#' 
#' @details 
#' Supported functions:
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
#' Supported classes:
#' \itemize{
#'   \item \code{confint}
#' }
#' 
#' Functions with additional arguments:
#' \itemize{
#'   \item \code{lavaan}
#' }
#' 
#' @examples 
#' # Load dplyr for access to the piping operator
#' library(dplyr)
#' 
#' # Conduct the analyses
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
#' #' # Create an empty list
#' statistics <- list()
#' 
#' # Add statistics to the list
#' statistics <- statistics %>%
#'   add_stats(sleep_test) %>%
#'   add_stats(lm_D9, type = "primary", preregistered = TRUE) %>%
#'   add_stats(npk_aov, notes = "An ANOVA example")
#' 
#' @export
add_stats <- function(list, output, identifier = NULL, type = NULL, 
  preregistered = NULL, notes = NULL, args = NULL, class = NULL) {
  UseMethod("add_stats", output)  
}

#' @export
add_stats.default <- function(list, output, identifier = NULL, type = NULL,
  preregistered = NULL, notes = NULL, args = NULL, class = NULL) {
  # Create an identifier if it is not specified, else check whether it already
  # exists
  if (is.null(identifier)) {
    if (deparse(substitute(output)) == ".") {
      identifier <- paste0("M", formatC(length(list) + 1, width = "1",
        format = "d"))
    } else {
      identifier <- deparse(substitute(output))
    }
  } else {
    if (!is.null(names(list))) {
      if (identifier %in% names(list)) {
        stop("Identifier already exists.")
      }
    }
  }
  
  # Add a class if one is provided
  if (!is.null(class)) {
    class(output) <- append(class(output), class, after = 0)
  }

  # Tidy the output
  analysis <- tidy_stats(output, args = args)

  # Add type: primary, secondary, or exploratory
  if (!is.null(type)) {
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
  
  # Add whether the analysis was preregistered or not
  if (!is.null(preregistered)) {
    if (preregistered) {
      analysis$preregistered <- "yes"
    } else {
      analysis$preregistered <- "no"
    }
  }
  
  # Add notes
  if (!is.null(notes)) {
    analysis$notes <- notes
  }

  # Add the new analysis to the list
  list[[identifier]] <- analysis

  # Return the new list
  return(list)
}

#' @export
add_stats.list <- function(list, output, identifier = NULL, type = NULL,
  preregistered = NULL, notes = NULL) {

  # Create an identifier if it is not specified, else check whether it already
  # exists
  if (is.null(identifier)) {
    if (deparse(substitute(output)) == ".") {
      identifier <- paste0("M", formatC(length(list) + 1, width = "1",
        format = "d"))
    } else {
      identifier <- deparse(substitute(output))
    }
  } else {
    if (!is.null(names(list))) {
      if (identifier %in% names(list)) {
        stop("Identifier already exists.")
      }
    }
  }
  
  # Run the default add_stats in case of a emm_list object
  if ("emm_list" %in% class(output)) {
    return(
      add_stats.default(list, output, identifier, type, preregistered, notes)
    )
  }

  # Simply set analysis to output; we don't need to tidy the output because
  # they should already be tidy
  analysis <- output
  
  # Add type: primary, secondary, or exploratory
  if (!is.null(type)) {
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
  
  # Add whether the analysis was preregistered or not
  if (!is.null(preregistered)) {
    if (preregistered) {
      analysis$preregistered <- "yes"
    } else {
      analysis$preregistered <- "no"
    }
  }
  
  # Add notes
  if (!is.null(notes)) {
    analysis$notes <- notes
  }

  # Add the new analysis to the list
  list[[identifier]] <- analysis

  # Return the new list
  return(list)
}
