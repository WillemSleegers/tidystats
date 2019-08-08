#' Create a tidy stats json object from an aov object
#'
#' @export

tidy_stats.aov <- function(model) {

  output <- list()
  
  # Get summary statistics
  summary <- summary(model)

  # Extract method
  output$method <- "ANOVA"
  
  # Create an empty coefficients list
  coefficients <- list()
  
  # Convert the summary statistics format to a data frame
  summary <- tibble::as_tibble(summary[[1]], rownames = "terms")
  
  # Trim spaces from the names of the terms
  summary <- mutate(summary, terms = stringr::str_trim(terms))
  
  for (i in 1:nrow(summary)) {
    
    # Create a new coefficient list
    coefficient <- list()
    
    # Add the name of the coefficient
    name = summary$terms[i]
    coefficient$name <- name
    
    # Create a new statistics list and add the coefficient's statistics
    statistics <- list()
    
    statistics$SS <- summary$`Sum Sq`[i]
    statistics$MS <- summary$`Mean Sq`[i]
    
    if (name != "Residuals") {
      statistic <- list()
      statistic$name <- "F"
      statistic$value <- summary$`F value`[i]
      statistics$statistic <- statistic
    }
    
    statistics$df <- summary$Df[i]
    
    if (name != "Residuals") {
      statistics$p <- summary$`Pr(>F)`[i]
    }
    
    coefficient$statistics <- statistics
    
    # Add the coefficient data to the coefficients list
    coefficients[[i]] <- coefficient
  }
  
  # Add coefficients to the output
  output$coefficients <- coefficients
  
  return(output)
}
