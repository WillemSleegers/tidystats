#' @describeIn tidy_stats tidy_stats method for class 'psych'
#' @export
tidy_stats.psych <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()
  
  # Check the kind of psych object
  if ("alpha" %in% class(x)) {
    # Set method
    analysis$method <- "Reliability analysis"
    
    # Create a statistics list for the total statistics and CI
    statistics <- list()
    
    statistics <- add_statistic(statistics, "unstandardized alpha", 
      x$total$raw_alpha, symbol = "α", subscript = "Σ")  
    statistics <- add_statistic(statistics, "standardized alpha", 
      x$total$std.alpha, symbol = "α", subscript = "R")
    statistics <- add_statistic(statistics, "Guttman's Lambda 6 reliability", 
      x$total$`G6(smc)`, symbol = "Guttman's λ", subscript = "6")
    statistics <- add_statistic(statistics, "mean interitem correlation", 
      x$total$average_r, symbol = "IIC", subscript = "M")
    statistics <- add_statistic(statistics, "signal-to-noise ratio", 
      x$total$`S/N`, symbol = "S/N")
    statistics <- add_statistic(statistics, "standard error", x$total$ase,
      symbol = "SE")
    statistics <- add_statistic(statistics, "mean", x$total$mean,
      symbol = "M")
    statistics <- add_statistic(statistics, "standard deviation", x$total$sd,
      symbol = "SD")
    statistics <- add_statistic(statistics, "median interitem correlation", 
      x$total$median_r, symbol = "IIC", subscript = "Mdn")
    
    # Add the statistics to the analysis
    analysis$statistics <- statistics
    
    # Create a group for the 95% confidence boundaries, if there are any
    if (!is.null(x$feldt) | !is.null(x$total$ase) | !is.null(x$boot.ci)) {
      group <- list(name = "95% confidence boundaries")
      
      if (!is.null(x$feldt)) {
        # Feldt group
        group_feldt <- list(name = "Feldt")
        statistics <- list()
        
        statistics <- add_statistic(statistics, "alpha", x$total$raw_alpha, 
          symbol = "α", interval = "CI", level = .95, 
          lower = x$feldt$lower.ci[[1]], upper = x$feldt$upper.ci[[1]])
        
        group_feldt$statistics <- statistics
        group$groups <- append(group$groups, list(group_feldt))
      }
      
      # Duhachek group
      if (!is.null(x$total$ase)) {
        group_duhachek <- list(name = "Duhachek")
        statistics <- list()
        
        statistics <- add_statistic(statistics, "alpha", x$total$raw_alpha, 
          symbol = "α", interval = "CI", level = .95, 
          lower = x$total$raw_alpha - 1.96 * x$total$ase, 
          upper = x$total$raw_alpha + 1.96 * x$total$ase)
        
        group_duhachek$statistics <- statistics
        group$groups <- append(group$groups, list(group_duhachek))
      }
      
      # Bootstrapped group
      if (!is.null(x$boot.ci)) {
        group_bootstrapped <- list(name = "bootstrapped")
        statistics <- list()
        
        statistics <- add_statistic(statistics, "alpha", x$boot.ci[2], 
          symbol = "α", interval = "CI", level = .95, 
          lower = x$boot.ci[1], upper = x$boot.ci[3])
        
        group_bootstrapped$statistics <- statistics
        group$groups <- append(group$groups, list(group_bootstrapped))
      }
      
      analysis$groups <- append(analysis$groups, list(group))
    }
    
    # Create a group for the the reliability if an item is dropped statistics
    group <- list(name = "Reliability if an item is dropped")
    
    # Loop over the items
    for (i in 1:nrow(x$alpha.drop)) {
      # Create a list for the item
      item <- list(name = rownames(x$alpha.drop)[i])
      
      # Create a statistics list and add the item statistics
      statistics <- list()
      
      statistics <- add_statistic(statistics, "unstandardized alpha", 
        x$total$raw_alpha[i], symbol = "α", subscript = "Σ")
      statistics <- add_statistic(statistics, "standardized alpha", 
        x$total$std.alpha[i], symbol = "α", subscript = "R")
      statistics <- add_statistic(statistics, "Guttman's Lambda 6 reliability", 
        x$total$`G6(smc)`, symbol = "Guttman's λ", subscript = "6")
      statistics <- add_statistic(statistics, "mean interitem correlation", 
        x$total$average_r[i], symbol = "IIC", subscript = "M")
      statistics <- add_statistic(statistics, "signal-to-noise ratio", 
        x$total$`S/N`[i], symbol = "S/N")
      statistics <- add_statistic(statistics, "standard error", x$total$ase[i],
        symbol = "SE")
      statistics <- add_statistic(statistics, "mean", x$total$mean[i],
        symbol = "M")
      statistics <- add_statistic(statistics, "standard deviation", 
        x$total$sd[i], symbol = "SD")
      statistics <- add_statistic(statistics, "variance interitem correlation", 
        x$total$var.r[i], symbol = "IIC", subscript = "var")
      statistics <- add_statistic(statistics, "median interitem correlation", 
        x$total$med.r[i], symbol = "IIC", subscript = "Mdn")
      
      # Add statistics to the group
      item$statistics <- statistics
      
      # Add item to the group
      group$groups <- append(group$groups, list(item))
    }
    
    # Add the reliability if an item is dropped group to the analysis groups
    analysis$groups <- append(analysis$groups, list(group))
    
    # Create a group for the item statistics
    group <- list(name = "Item statistics")
    
    # Loop over the items
    for (i in 1:nrow(x$item.stats)) {
      # Create a list for the item
      item <- list(name = rownames(x$item.stats)[i])
      
      # Create a statistics list and add the item statistics
      statistics <- list()
      
      statistics <- add_statistic(statistics, "number of complete cases", 
        x$item.stats$n[i], symbol = "n")
      statistics <- add_statistic(statistics, 
        "total score correlation with standardized items", 
        x$item.stats$r[i], symbol = "r", subscript = "std")
      statistics <- add_statistic(statistics, "total score correlation", 
        x$item.stats$raw.r[i], symbol = "r", subscript = "raw")
      statistics <- add_statistic(statistics, 
        "total score correlation with standardized items", 
        x$item.stats$std.r[i], symbol = "r", subscript = "std")
      statistics <- add_statistic(statistics, 
        "corrected item whole correlation", 
        x$item.stats$r.cor[i], symbol = "r", subscript = "cor")
      statistics <- add_statistic(statistics, 
        "item whole correlation without this item", 
        x$item.stats$r.drop[i], symbol = "r", subscript = "drop")
      statistics <- add_statistic(statistics, "mean", x$item.stats$mean[i],
        symbol = "M")
      statistics <- add_statistic(statistics, "standard deviation", 
        x$item.stats$sd[i], symbol = "SD")
      
      # Add statistics to the group
      item$statistics <- statistics
      
      # Add item to the group
      group$groups <- append(group$groups, list(item))
    }
    
    # Add the reliability if an item is dropped group to the analysis groups
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  if ("corr.test" %in% class(x)) {
    analysis$method <- "Correlation" #TODO detect kendall or spearman
    
    if (x$adjust == "none") {
      warning("Only saving unadjusted statistics.")
    } else {
      warning("Only saving adjusted statistics.")
    }
    
    # Check if there is only 1 pair, or multiple
    if (length(rownames(x$r)) == 1) {
      # Create a list for the statistics of this single pair
      statistics <- list()
      
      #TODO: figure out number of pairs
      
    }
    
    analysis$adjust <- x$adjust
  }
  
  if ("mardia" %in% class(x)) {
    # Set method
    analysis$method <- "Mardia's test"
    
    # Create a statistics list for number of observations and variables
    statistics <- list()
    
    statistics <- add_statistic(statistics, "number of observations", x$n.obs, 
      symbol = "N")
    statistics <- add_statistic(statistics, "number of variables", x$n.var, 
      symbol = "k")
    
    analysis$statistics <- statistics
    
    # Create a group for the skew statistics
    group <- list(name = "skew")
    
    statistics <- list()
    
    statistics <- add_statistic(statistics, "estimate", x$b1p, symbol = "b", 
      subscript = "1, p")
    statistics <- add_statistic(statistics, "skew", x$skew)
    statistics <- add_statistic(statistics, "p", x$p.skew)
    
    group$statistics <- statistics
    analysis$groups <- append(analysis$groups, list(group))
    
    # Create a group for the small sample skew statistics
    group <- list(name = "small sample skew")
    
    statistics <- list()
    
    statistics <- add_statistic(statistics, "estimate", x$b1p, symbol = "b", 
      subscript = "1, p")
    statistics <- add_statistic(statistics, "skew", x$small.skew)
    statistics <- add_statistic(statistics, "p", x$p.small)
    
    group$statistics <- statistics
    analysis$groups <- append(analysis$groups, list(group))
    
    # Create a group for the kurtosis statistics
    group <- list(name = "kurtosis")
    
    statistics <- list()
    
    statistics <- add_statistic(statistics, "estimate", x$b2p, symbol = "b", 
      subscript = "2, p")
    statistics <- add_statistic(statistics, "kurtosis", x$kurtosis)
    statistics <- add_statistic(statistics, "p", x$p.kurt)
    
    group$statistics <- statistics
    analysis$groups <- append(analysis$groups, list(group))
  }
  
  # Add package information
  analysis <- add_package_info(analysis, "psych")
  
  return(analysis)
}
