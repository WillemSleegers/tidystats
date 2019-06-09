#' Add statistical output to a tidy stats list
#'
#' \code{add_stats} adds output to a tidystats list. It can take either the
#' output of a statistical test as input or a data frame. See Details for more
#' information on adding data frames.
#'
#' @param results A tidystats list.
#' @param output Output of a statistical test or a data frame. If a data frame
#' is provided, it must already be in a tidy format.
#' @param identifier A character string identifying the model. Automatically
#' created if not provided.
#' @param notes A character string to add additional information. Some
#' statistical tests produce notes information, which will be overwritten if
#' notes are provided.
#' @param class A character string to indicate which function was used to
#' produce the output. See 'Details' for a list of supported functions.
#' @param args An optional list of additional arguments. Can be used to specify
#' how model results should be summarized.
#'
#' @details Some statistical functions produce unidentifiable output, which
#' means \code{tidystats} cannot figure out how to parse the data. To add these
#' results, you can provide a class via the class argument or you can manually
#' tidy the results yourself and add the resulting data frame via add_stats().
#'
#' A list of supported classes are:
#' - \code{confint}
#'
#' @export
add_stats <- function(results, output, identifier = NULL, notes = NULL, 
  class = NULL, args = NULL) UseMethod("add_stats", output)
