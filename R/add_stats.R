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
#'   \item \code{anova()}
#'   \item \code{ansari.test()}
#'   \item \code{aov()}
#'   \item \code{bartlett.test()}
#'   \item \code{binom.test()}
#'   \item \code{Box.test()}
#'   \item \code{chisq.test()}
#'   \item \code{cor.test()}
#'   \item \code{fisher.test()}
#'   \item \code{fligner.test()}
#'   \item \code{friedman.test()}
#'   \item \code{glm()}
#'   \item \code{kruskal.test()}
#'   \item \code{ks.test()}
#'   \item \code{lm()}
#'   \item \code{mantelhaen.test()}
#'   \item \code{mauchly.test()}
#'   \item \code{mcnemar.test()}
#'   \item \code{mood.test()}
#'   \item \code{oneway.test()}
#'   \item \code{pairwise.prop.test()}
#'   \item \code{pairwise.t.test()}
#'   \item \code{pairwise.wilcox.test()}
#'   \item \code{poisson.test()}
#'   \item \code{PP.test()}
#'   \item \code{prop.test()}
#'   \item \code{prop.trend.test()}
#'   \item \code{quade.test()}
#'   \item \code{shapiro.test()}
#'   \item \code{t.test()}
#'   \item \code{var.test()}
#'   \item \code{wilcox.test()}
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
add_stats <- function(list, output, identifier = NULL, type = NULL,
  preregistered = NULL, notes = NULL, args = NULL, class = NULL)
    UseMethod("add_stats", output)

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

  # Add whether the analysis was preregistered or not
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

  # Simply set analysis to output; we don't need to tidy the output because
  # they should already be tidy
  analysis <- output

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

  # Add whether the analysis was preregistered or not
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
  list[[identifier]] <- analysis

  # Return the new list
  return(list)
}
