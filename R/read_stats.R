#' Read a .json file that was produced with \code{write_stats}
#'
#' \code{read_stats} can read in a .json file containing the statistical output
#' that was produced with \code{write_stats}. It returns a list containing the
#' results, with the identifier as the name for each list element.
#'
#' @param file A string; specifying the path to the tidystats data file.
#'
#' @examples
#' # A simple example, assuming there is a file called 'results.json'
#' \dontrun{
#'   results <- read_stats("results.json")
#' }
#'
#' # A working example
#' results <- read_stats(system.file("results.json", package = "tidystats"))
#'
#' @export
read_stats <- function(file) {
  # Check the file extension and throw a warning if it's not a .json file
  if (tools::file_ext(file) != "json") {
    warning(
      paste(
        "The file does not have a .json file extension;",
        "make sure to check whether you have specified the correct file."
      )
    )
  }

  # Use the jsonlite read_json function to read in the data
  results <- jsonlite::read_json(file)

  # Look for character Inf's and NAs and convert them to numeric
  results <- rapply(results, function(x) if (x == "Inf") Inf else if (x == "NA") as.numeric(NA) else x,
    how = "replace")

  return(results)
}
