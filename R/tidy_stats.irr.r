#' @describeIn tidy_stats tidy_stats method for class 'icclist'
#' @export
tidy_stats.icclist <- function(x, args = NULL) {
  analysis <- list(method = "ICC")
  
  statistics <- list() %>%
    add_statistic("N subjects", x$subjects, "N", "subjects") %>%
    add_statistic("N raters", x$raters, "N", "raters") %>%
    add_statistic(
      name = x$icc.name, 
      value = x$value, 
      interval = "CI", 
      level = x$conf.level, 
      lower = x$lbound, 
      upper = x$ubound
    ) %>%
    add_statistic("statistic", x$Fvalue, "F") %>%
    add_statistic("df numerator", x$df1, "df", "num.") %>%
    add_statistic("df denominator", x$df2, "df", "den.") %>%
    add_statistic("p", x$p.value)
  
  analysis$statistics <- statistics
  
  analysis$model <- x$model
  analysis$type <- x$type
  analysis$unit <- x$unit
  
  analysis$alernative <- list(null_value = x$r0)
  
  analysis <- add_package_info(analysis, "irr")
  
  return(analysis)
}
