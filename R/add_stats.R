#' Add statistical output to a tidystats list
#'
#' \code{add_stats} is used to add the output of a statistical test to a 
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
add_stats <- function(results, output, identifier = NULL, type = NULL, 
  preregistered = NULL, notes = NULL) UseMethod("add_stats", output)
