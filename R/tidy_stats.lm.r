#' Create a tidy stats json object from an lm object
#' 
#' @export
tidy_stats.lm <- function(model) {

  output <- list()
  
  # Get summary statistics
  summary <- summary(model)

  # Extract method
  output$method <- "Linear regression"
  
  # Extract statistics of the coefficients
  coef <- coef(summary)
  
  # Create an empty coefficients list
  coefficients <- list()
  
  for (i in 1:nrow(coef(summary))) {
    
    # Create a new coefficient list
    coefficient <- list()
    
    # Add the name of the coefficient
    name = rownames(coef)[i]
    coefficient$name <- name
    
    # Create a new statistics list and add the coefficient's statistics
    statistics <- list()
    
    statistics$estimate <- coef[name, "Estimate"]
    statistics$SE <- coef[name, "Std. Error"]
    
    statistic <- list()
    statistic$name <- "t"
    statistic$value <- coef[name, "t value"]
    statistics$statistic <- statistic
    
    statistics$df <- summary$df[2]
    
    statistics$p <- coef[name, "Pr(>|t|)"]
    
    coefficient$statistics <- statistics
    
    # Add the coefficient data to the coefficients list
    coefficients[[i]] <- coefficient
  }
  
  # Extract model statistics
  model <- list()
  statistics <- list()
  
  statistics$r_squared <- summary$r.squared
  statistics$adjusted_r_squared <- summary$adj.r.squared
  
  statistic <- list()
  statistic$name <- "F"
  statistic$value <- summary$fstatistic[[1]]
  statistics$statistic <- statistic
  
  dfs <- list()
  dfs$numerator_df <- summary$fstatistic[[2]]
  dfs$denominator_df <- summary$fstatistic[[3]]
  statistics$dfs <- dfs
  
  statistics$p <- pf(summary$fstatistic[[1]], summary$fstatistic[[2]], 
    summary$fstatistic[[3]], lower.tail = FALSE)
  statistics$sigma <- summary$sigma
  
  model$statistics <- statistics
  
  # Add coefficients and model to the output
  output$coefficients <- coefficients
  output$model <- model
  
  return(output)
}
