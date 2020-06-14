#' Read a .json file that was produced with \code{write_stats}
#'
#' \code{read_stats} can read in a .json file containing the statistical output
#' that was produced with \code{write_stats}. It returns a list containing the 
#' results, with the identifier as the name for each list element.
#'
#' @param file Path to the tidy stats data file
#'
#' @examples
#' results <- read_stats(system.file("results.json", package = "tidystats"))
#'
#' @export
read_stats <- function(file) {

  # Read a tidystats .json file
  results <- jsonlite::read_json(file)

  # Look for character Inf's and convert them to numeric
  results <- rapply(results, function(x) if(x == "Inf") Inf else x, 
    how = "replace")
  
  return(results)
}
