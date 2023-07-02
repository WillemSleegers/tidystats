#' Read a .json file that was produced with [write_stats()]
#'
#' [read_stats()] can read a .json file containing statistics that was produced
#' using tidystats. It returns a list containing the statistics, with the
#' identifier as the name for each list element.
#'
#' @param file A string specifying the path to the tidystats data file.
#'
#' @examples
#' # A simple example, assuming there is a file called 'statistics.json'
#' \dontrun{
#' statistics <- read_stats("statistics.json")
#' }
#'
#' # A working example
#' statistics <- read_stats(
#'   file = system.file("statistics.json", package = "tidystats")
#' )
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
