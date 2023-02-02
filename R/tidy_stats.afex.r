#' @describeIn tidy_stats tidy_stats method for class 'afex_aov'
#' @export
tidy_stats.afex_aov <- function(x, args = NULL) {
  # Create the analysis list and set the name and method
  analysis <- list(method = "ANOVA")
  
  # Get term statistics
  terms <- x$anova_table
  
  # Create an empty groups list to add term statistics to
  groups <- list(name = "Terms")
  
  # Loop over the terms 
  for (i in 1:nrow(terms)) {
    # Create a new group list
    group <- list(name = rownames(terms)[i])
    
    # Create a new statistics list and add the term's statistics
    statistics <- list()
    
    statistics <- add_statistic(
      list = statistics, 
      name = "df numerator", 
      value = terms$`num Df`[i],
      symbol = "df", 
      subscript = "num."
    )
    statistics <- add_statistic(
      list = statistics, 
      name = "df denominator", 
      value = terms$`den Df`[i],
      symbol = "df", 
      subscript = "den."
    )
    statistics <- add_statistic(statistics, "MSE", terms$MSE[i])
    statistics <- add_statistic(statistics, "statistic", terms$`F`[i], "F")
    statistics <- add_statistic(statistics, "ges", terms$ges[i], "η²", "G")
    statistics <- add_statistic(statistics, "pes", terms$pes[i], "η²", "p")
    statistics <- add_statistic(statistics, "p", terms$`Pr(>F)`[i])
    
    # Add statistics to the group
    group$statistics <- statistics
    
    # Add the group to the groups list
    groups$groups <- append(groups$groups, list(group))
  }
  
  # Add the groups to the groups list on the analysis list
  analysis$groups <- append(analysis$groups, list(groups))
  
  # Add additional information
  analysis$anova_type <- attr(x, "type")
  analysis$p_adjustment_method <- attr(x$anova_table, "p_adjust_method")
  analysis$sphericity_correction_method <- attr(x$anova_table, "correction")
  
  # Add package information
  analysis <- add_package_info(analysis, "afex")
  
  return(analysis)
}