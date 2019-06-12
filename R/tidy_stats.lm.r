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
  coefficients <- list()
  
  for (name in rownames(coef(summary))) {
    statistics <- as.list(coef(summary)[name, ])
    coefficients[[name]]$statistics <- statistics
  }
  
  # Extract model statistics
  statistics <- list()
  
  statistics$r.squared <- summary$r.squared
  statistics$adj.r.squared <- summary$adj.r.squared
  statistics$fstatistic <- summary$fstatistic[[1]]
  statistics$numdf <- summary$fstatistic[[2]]
  statistics$dendf <- summary$fstatistic[[3]]
  statistics$p <- pf(summary$fstatistic[[1]], summary$fstatistic[[2]], 
    summary$fstatistic[[3]], lower.tail = FALSE)
  statistics$sigma <- summary$sigma
  
  model <- list()
  model$statistics <- statistics
  
  # Add coefficients and model to the output
  output$coefficients <- coefficients
  output$model <- model
  
  return(output)
}
