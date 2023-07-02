#' Create a collection of custom statistics
#'
#' [custom_stats()] is used to create a collection of statistics from
#' unsupported functions to add to a list via [add_stats()].
#'
#' @param method A string specifying the method used to obtain the statistics.
#' @param statistics A vector of statistics created with [custom_stat()].
#'
#' @details
#' [custom_stats()] supports adding a single statistic or a group of statistics.
#' Multiple groups of statistics are not (yet) supported.
#'
#' @examples
#' # Example: BIC Bayes factor (approx.)
#' # Run the analysis
#' lm1 <- lm(Fertility ~ ., data = swiss)
#' lm2 <- update(lm1, . ~ . - Examination)
#'
#' BF10 <- 1 / exp((BIC(lm2) - BIC(lm1)) / 2)
#'
#' # Create the custom statistics
#' BIC_BFs <- custom_stats(
#'   method = "BIC Bayes factor",
#'   statistics = c(
#'     custom_stat(name = "BF", value = BF10, subscript = "10"),
#'     custom_stat(name = "BF", value = 1 / BF10, subscript = "01")
#'   )
#' )
#'
#' # Create an empty list
#' statistics <- list()
#'
#' # Add the custom statistics to the list
#' statistics <- add_stats(statistics, BIC_BFs)
#'
#' @export
custom_stats <- function(method, statistics) {
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
