#' Write a tidy stats list to a file
#'
#' \code{write_stats} writes a tidy stats list to a .json file.
#'
#' @param results A tidy stats list.
#' @param path Path or connection to write to.
#'
#' @importFrom jsonlite write_json
#'
#' @export
write_stats <- function(results, path) {

  # Check whether the arguments are supplied
  if (!is.list(results)) {
    stop("argument 'results' is not a list")
  }
  if (is.null(path)) {
    stop()
  }

  # Write to disk
  jsonlite::write_json(results, path = path, pretty = TRUE)
}
