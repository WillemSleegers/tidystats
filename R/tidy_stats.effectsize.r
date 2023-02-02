#' @describeIn tidy_stats tidy_stats method for class 'effectsize_difference'
#' @export
tidy_stats.effectsize_difference <- function(x, args = NULL) {
  analysis <- list()
  
  method <- dplyr::case_when(
    "Cohens_d" %in% names(x) ~ "Cohen's d",
    "Hedges_g" %in% names(x) ~ "Hedge's g",
    "Glass_delta" %in% names(x) ~ "Glass' delta"
  )
  analysis$method <- method
   
  statistics <- list()
  statistics <- add_statistic(
    list = statistics, 
    name = method, 
    value = x[[1]], 
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