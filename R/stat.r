#' Create a custom statistic
#'
#' `stat()` can be used to create your own statistic to add to a custom
#' statistics object using [stats()].
#'
#' @param name A string specifying the name of the statistic.
#' @param value The numeric value of the statistic.
#' @param symbol A string specifying the symbol of the statistic to use when
#'   reporting the statistic.
#' @param subscript A string specifying a subscript to use when reporting the
#'   statistic.
#' @param interval A string specifying the type of interval if the statistic is
#'   a ranged statistic (e.g., one with confidence intervals)/
#' @param level A numeric value between 0 and 1 indicating the level of the
#'   interval.
#' @param lower The numeric value of the lower bound of the statistic.
#' @param upper The numeric value of the upper bound of the statistic.
#'
#' @examples
#' # Example: Number of observations
#' stat(name = "N", value = nrow(sleep))
#'
#' # Example: Mean and standard error
#' # (Note that you can use the describe_data() function to calculate these 
#'   statistics more easily)
#' sample <- rnorm(1000, mean = 4, sd = 1)
#' mean <- mean(sample)
#' se <- sd(sample) / sqrt(length(sample))
#'
#' stat(name = "mean", value = mean, symbol = "M")
#' stat(name = "standard error", value = se, symbol = "SE")
#'
#' # Example: Mean with a 95% confidence interval
#' CI <- c(mean - 1.96 * se, mean + 1.96 * se)
#' stat(
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
stat <- function(name, value, symbol = NULL, subscript = NULL,
                 interval = NULL, level = NULL, lower = NULL,
                 upper = NULL) {
  stat <- list(
    name = name,
    value = value
  )

  if (!is.null(symbol)) stat$symbol <- symbol
  if (!is.null(subscript)) stat$subscript <- subscript

  if (!is.null(interval)) {
    stat$interval <- interval
    stat$level <- level
    stat$lower <- lower
    stat$upper <- upper
  }

  class(stat) <- c("tidystats", "list")

  return(list(stat))
}
