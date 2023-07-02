#' Create a custom statistic
#'
#' [custom_stat()] is used together with the [custom_stats()] function to add
#' statistics from unsupported functions via [add_stats()]. See the
#' [custom_stats()] function for more information.
#'
#' @param name A string specifying the name of the statistic.
#' @param value The numeric value of the statistic.
#' @param symbol A string specifying the symbol of the statistic to use when
#'   reporting the statistic.
#' @param subscript A string specifying a subscript to use when reporting the
#'   statistic.
#' @param interval A string specifying the type of interval if the statistic is
#'   a ranged statistic (e.g., 95% confidence interval)
#' @param level A numeric value between 0 and 1 indicating the level of the
#'   interval.
#' @param lower The numeric value of the lower bound of the statistic.
#' @param upper The numeric value of the upper bound of the statistic.
#'
#' @examples
#' # Example 1: A single mean value
#' sample <- rnorm(1000, mean = 0, sd = 1)
#' mean <- mean(sample)
#'
#' custom_stat(name = "mean", value = mean, symbol = "M")
#'
#' # Example 2: A mean with a 95% confidence interval
#' sample <- rnorm(1000, mean = 0, sd = 1)
#' mean <- mean(sample)
#' se <- sd(sample) / sqrt(length(sample))
#' CI <- c(mean - 1.96 * se, mean + 1.96 * se)
#'
#' custom_stat(
#'   name = "mean",
#'   value = mean,
#'   symbol = "M",
#'   interval = "CI",
#'   level = .95,
#'   lower = CI[1],
#'   upper = CI[2]
#' )
#'
#' @export
custom_stat <- function(name, value, symbol = NULL, subscript = NULL,
                        interval = NULL, level = NULL, lower = NULL,
                        upper = NULL) {
  custom_stat <- list(
    name = name,
    value = value
  )

  if (!is.null(symbol)) custom_stat$symbol <- symbol
  if (!is.null(subscript)) custom_stat$subscript <- subscript

  if (!is.null(interval)) {
    custom_stat$interval <- interval
    custom_stat$level <- level
    custom_stat$lower <- lower
    custom_stat$upper <- upper
  }

  class(custom_stat) <- c("tidystats", "list")

  return(list(custom_stat))
}
