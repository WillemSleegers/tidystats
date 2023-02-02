#' @describeIn tidy_stats tidy_stats method for class 'marginaleffects'
#' @export
tidy_stats.marginaleffects <- function(x, args = NULL) {
  analysis <- list(method = "Marginal effects")

  vars <- names(attr(x, "variables"))

  return(analysis)
}
