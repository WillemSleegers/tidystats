#' Read a .json file that was produced with \code{write_stats}
#'
#' `read_stats()` can read in a .json file containing the statistical output
#' that was produced with [write_stats()]. It returns a list containing the
#' results, with the identifier as the name for each list element.
#'
#' @param file A string; specifying the path to the tidystats data file.
#'
#' @examples
#' # A simple example, assuming there is a file called 'results.json'
#' \dontrun{
#' results <- read_stats("results.json")
#' }
#'
#' # A working example
#' results <- read_stats(system.file("results.json", package = "tidystats"))
#'
#' @export
read_stats <- function(file) {
  if (tools::file_ext(file) != "json") {
    warning(
      paste(
        "The file does not have a .json file extension;",
        "make sure you have specified the correct file."
      )
    )
  }

  results <- jsonlite::read_json(file)

  # Look for character Inf's and convert them to numeric
  results <- rapply(
    results,
    function(x) ifelse(x %in% c("Inf", "-Inf"), as.numeric(x), x),
    how = "replace"
  )

  return(results)
}
