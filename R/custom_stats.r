#' Create a custom statistics object
#'
#' \code{custom_stats} can be used to create your own statistics object to add
#' to a statistics list via \code{add_stats()}. This can be used to add a single
#' statistic or a group of statistics. Note that multiple groups of statistics
#' are not supported.
#'
#' @param method A string; specifying the method used to obtain the statistics.
#' @param statistics A vector of statistics created with \code{stat()}.
#'
#' @examples
#' # Example of a calculating a custom statistic and adding it to a tidystats
#' # list
#' lm1 <- lm(Fertility ~ ., data = swiss)
#' lm2 <- update(lm1, . ~ . - Examination)
#'
#' BF10 <- 1 / exp((BIC(lm2) - BIC(lm1)) / 2)
#'
#' BIC_BFs <- custom_stats(
#'   method = "Wagenmakers (2007) method for calculating Bayes factors",
#'   statistics = c(
#'     stat(name = "BF", value = BF10, subscript = "10"),
#'     stat(name = "BF", value = 1 / BF10, subscript = "01")
#'   )
#' )
#'
#' statistics <- list()
#'
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
