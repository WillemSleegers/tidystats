#' @describeIn tidy_stats tidy_stats method for class 'effsize'
tidy_stats.effsize <- function(x, args = NULL) {
  analysis <- list(method = paste(x$method, "effect size"))
  statistics <- list()

  # Determine the symbol, which is different from what is stored in the name
  # attribute in the case of a Cliff's Delta
  symbol <- dplyr::if_else(
    x$name == "delta",
    intToUtf8(0x03b4),
    x$name
  )

  statistics <- add_statistic(
    list = statistics,
    name = x$method,
    symbol = symbol,
    value = x$estimate,
    interval = "CI",
    level = x$conf.level,
    lower = x$conf.int[["lower"]],
    upper = x$conf.int[["upper"]]
  )

  analysis$statistics <- statistics

  analysis <- add_package_info(analysis, "effsize")

  return(analysis)
}
