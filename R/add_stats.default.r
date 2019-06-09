#' add_stats default function
#'
#' \code{add_stats.default} is the default add_stats function, which takes the
#' output of a statistical test, organizes the output, and adds it to a tidystats
#' list.
#'
#' @param results A tidy stats list.
#' @param output Output of a statistical test.
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
#' @export

add_stats.default <- function(results, output, identifier = NULL, notes = NULL,
  class = NULL, args = NULL) {

  # Create an identifier if it is not specified, else check whether it already
  # exists
  if (is.null(identifier)) {

    if (deparse(substitute(output)) == ".") {
      identifier <- paste0("M", formatC(length(results)+1, width = "1",
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

  # Create the new element
  new_element <- tidy_stats(output)

  # Add notes
  if (!is.null(notes)) {
    new_element$notes <- notes
  }

  # Add the new element to the list
  results[[identifier]] <- new_element

  # Return the new results list
  return(results)
}
