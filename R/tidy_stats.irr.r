#' @describeIn tidy_stats tidy_stats method for class 'icclist'
#' @export
tidy_stats.icclist <- function(x, args = NULL) {
  # Create the analysis list and set the method
  analysis <- list(method = "ICC")
  
  # Extract statistics
  statistics <- list()
  
  statistics <- add_statistic(statistics, "N subjects", x$subjects, "N", 
    "subjects")
  statistics <- add_statistic(statistics, "N raters", x$raters, "N", "raters")
  statistics <- add_statistic(statistics, "ICC", x$value, interval = "CI", 
    level = x$conf.level, lower = x$lbound, upper = x$ubound)
  statistics <- add_statistic(statistics, "statistic", x$Fvalue, "F")
  statistics <- add_statistic(statistics, "df numerator", x$df1, "df", "num.")
  statistics <- add_statistic(statistics, "df denominator", x$df2, "df", "den.")
  statistics <- add_statistic(statistics, "p", x$p.value)
  
  # Add statistics to the analysis
  analysis$statistics <- statistics
  
  # Add additional information
  analysis$model <- x$model
  analysis$type <- x$type
  analysis$unit <- x$unit
  analysis$ICC_name <- x$icc.name
  
  alternative <- list(null_value = x$r0)
  analysis$alernative <- alternative
  
  # Add package information
  analysis <- add_package_info(analysis, "irr")
  
  return(analysis)
}
