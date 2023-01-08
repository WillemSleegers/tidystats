#' @describeIn tidy_stats tidy_stats method for class 'rcorr'
#' @export
tidy_stats.rcorr <- function(x, args = NULL) {
  message(
    paste(
      "Can't determine whether the correlations are Pearson's r or Spearman's",
      "rho rank correlation coefficients."
    )
  )
  
  analysis <- list(method = "Correlation")
  
  rs <- tidy_matrix(x$r)
  ns <- tidy_matrix(x$n)
  ps <- tidy_matrix(x$P)
  
  groups <- list(name = "Pairs")
  
  for (i in 1:nrow(rs)) {
    names <- list(
      list(name = rs$name1[i]),
      list(name = rs$name2[i])
    )
    
    group <- list(names = names)
    statistics <- list()
    
    statistics <- add_statistic(statistics, name = "r", value = rs$value[i])
    statistics <- add_statistic(statistics, name = "n", value = ns$value[i])
    statistics <- add_statistic(statistics, name = "p", value = ps$value[i])
    
    group$statistics <- statistics
    groups$groups <- append(groups$groups, list(group))
  }
  
  analysis$groups <- append(analysis$groups, list(groups))
  
  analysis <- add_package_info(analysis, "afex")
  
  return(analysis)
}
