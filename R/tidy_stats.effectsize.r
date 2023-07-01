#' @describeIn tidy_stats tidy_stats method for class 'effectsize_difference'
#' @keywords internal
tidy_stats.effectsize_difference <- function(x, args = NULL) {
  analysis <- list()

  if ("Cohens_d" %in% names(x)) {
    method <- "Cohen's d effect size"
    name <- "Cohen's d"
    symbol <- "d"
  } else if ("Hedges_g" %in% names(x)) {
    method <- "Hedge's g effect size"
    name <- "Hedge's g"
    symbol <- "g"
  } else {
    method <- "Glass' delta effect size"
    name <- "Glass' delta"
    symbol <- "δ"
  }

  analysis$method <- method

  statistics <- list()
  statistics <- add_statistic(
    list = statistics,
    name = name,
    value = x[[1]],
    symbol = symbol,
    interval = "CI",
    level = x[[2]],
    lower = x[[3]],
    upper = x[[4]]
  )
  analysis$statistics <- statistics

  analysis$alternative <- list(
    direction = attr(x, "alternative"),
    null_value = attr(x, "mu")
  )
  analysis$paired <- attr(x, "paired")
  analysis$correction <- attr(x, "correction")
  analysis$pooled_sd <- attr(x, "pooled_sd")
  analysis$proximate <- attr(x, "approximate")

  analysis <- add_package_info(analysis, "effectsize")

  return(analysis)
}
