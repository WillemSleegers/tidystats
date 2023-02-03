#' Create a custom statistics object
#'
#' \code{stats} can be used to create your own statistics object to add to a
#' statistics list via \code{add_stats()}.
#'
#' @param method A string; specifying the path to type of method used to obtain
#'   the statistics.
#' @param statistics A vector of statistics created with \code{stat()}.
#'
#' @export
stats <- function(method, statistics) {
  stats <- list()
  for (stat in statistics) {
    stats <- append(stats, list(stat))
  }

  output <- list(
    method = method,
    statistics = stats
  )

  class(output) <- c("tidystats", "list")

  return(output)
}
