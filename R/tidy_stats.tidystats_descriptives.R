#' Convert a tidystats_descriptives data frame to a list
#'
#' \code{tidy_stats.tidystats_descriptives} converts a data frame created with
#' \code{describe_data} to a list that can, in turn, be converted to a .json 
#' file.
#' 
#' @param data A data frame created with \code{describe_data}.
#' 
#' @details When the data frame also contains grouping variables, please make
#' sure that the data frame is grouped by those variables using \strong{dplyr}'s 
#' \code{group_by}. This is automatically the case when the output of 
#' \code{describe_data} is used.
#' 
#' @export

tidy_stats.tidystats_descriptives <- function(data) {

  output <- list()
  
  # Add method
  output$method <- "Descriptives"
  
  # Extract variable information
  var_names <- unique(pull(data, variable))
  n_vars <- length(var_names)
  
  # Extract grouping information
  group_names <- group_vars(data)
  n_groups <- length(group_names)
  
  # Create a variables list and loop over the variables, extracting the relevant
  # statistics
  variables <- list()
  
  for (i in 1:n_vars) {
    # Create an empty variable list and set the name property
    variable <- list()
    variable$name <- var_names[i]
    
    # Extract only the rows belonging to the current variable
    rows <- dplyr::filter(data, variable == var_names[i])
  
    # Check whether there are any grouping variables and select the relevant
    # row of descriptives
    if (n_groups > 0) {
      
      # Set the grouping name
      # If there is more than 1 grouping variable, combine them together
      variable$group_by = paste(group_names, collapse = " by ")
      
      # Create an empty groups list
      groups <- list()
      
      # Loop over the groups
      for (j in 1:nrow(rows)) {
        # Create an empty group list
        group <- list()
        
        # Select the current row
        row <- rows[j, ]
        
        # Set the group name
        group$name <- paste(unlist(row[group_names]), collapse = " - ")
        
        # Extract statistics
        statistics <- list()
      
        if ("missing" %in% names(row)) statistics$missing <- row$missing
        if ("N" %in% names(row)) statistics$N <- row$N
        if ("M" %in% names(row)) statistics$M <- row$M
        if ("SD" %in% names(row)) statistics$SD <- row$SD
        if ("SE" %in% names(row)) statistics$SE <- row$SE
        if ("min" %in% names(row)) statistics$min <- row$min
        if ("max" %in% names(row)) statistics$max <- row$max
        if ("range" %in% names(row)) statistics$range <- row$range
        if ("median" %in% names(row)) statistics$median <- row$median
        if ("mode" %in% names(row)) statistics$mode <- row$mode
        if ("skew" %in% names(row)) statistics$skew <- row$skew
        if ("kurtosis" %in% names(row)) statistics$kurtosis <- row$kurtosis
        
        # Add the statistics to the variable's statistics property
        group$statistics <- statistics    
        
        # Add the group to the groups list
        groups[[j]] <- group
      }
      
      # Add the groups list to the variable list
      variable$groups <- groups
      
    } else {
      row <- data[i, ]
      
      # Extract statistics
      statistics <- list()
    
      statistics$missing <- row$missing
      statistics$N <- row$N
      statistics$M <- row$M
      statistics$SD <- row$SD
      statistics$SE <- row$SE
      statistics$min <- row$min
      statistics$max <- row$max
      statistics$range <- row$range
      statistics$median <- row$median
      statistics$mode <- row$mode
      statistics$skew <- row$skew
      statistics$kurtosis <- row$kurtosis
      
      # Add the statistics to the variable's statistics property
      variable$statistics <- statistics
    }
  
    # Add variable to the list of variables
    variables[[i]] <- variable
  }
  
  # Add variables to the output
  output$variables <- variables
  
  # Add package information
  package <- list()

  package$name <- "tidystats"
  package$version <- getNamespaceVersion("tidystats")[[1]]

  # Add package information to output
  output$package <- package
  
  return(output)
}


