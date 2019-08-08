#' Create a tidy stats json object from an aovlist object
#'
#' @export

tidy_stats.aovlist <- function(model) {

  output <- list()
  
  # Get summary statistics
  summary <- summary(model)

  # Extract method
  output$method <- "ANOVA"
  
  # Create an empty groups list
  groups <- list()
  
  for (i in 1:length(summary)) {
    
    # Create an empty group
    group <- list()
    
    # Set the group name
    group$name <- names(summary[i])
    
    # Create an empty coefficients list
    coefficients <- list()
    
    # Convert the summary statistics format to a data frame
    summary_df <- as_tibble(as.data.frame(as.list.data.frame(summary[[i]])), 
      rownames = "terms")
    
    # Trim spaces from the names of the terms
    summary_df <- mutate(summary_df, terms = stringr::str_trim(terms))
    
    for (j in 1:nrow(summary_df)) {
    
      # Create a new coefficient list
      coefficient <- list()
      
      # Add the name of the coefficient
      name = summary_df$terms[j]
      coefficient$name <- name
      
      # Create a new statistics list and add the coefficient's statistics
      statistics <- list()
      
      statistics$SS <- summary_df$`Sum.Sq`[j]
      statistics$MS <- summary_df$`Mean.Sq`[j]
      
      if (name != "Residuals") {
        statistic <- list()
        statistic$name <- "F"
        statistic$value <- summary_df$`F.value`[j]
        statistics$statistic <- statistic
      }
      
      statistics$df <- summary_df$Df[j]
      
      if (name != "Residuals") {
        statistics$p <- summary_df$Pr..F.[j]
      }
    
      coefficient$statistics <- statistics
    
      # Add the coefficient data to the coefficients list
      coefficients[[j]] <- coefficient
    }
    
    # Add coefficients to the group
    group$coefficients <- coefficients
    
    # Add group to the groups list
    groups[[i]] <- group
  }
 
  # Add groups to the output
  output$groups <- groups
 
  return(output)
}
