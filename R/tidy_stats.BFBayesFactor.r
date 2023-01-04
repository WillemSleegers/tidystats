#' @describeIn tidy_stats tidy_stats method for class 'BayesFactor'
#' @export
tidy_stats.BFBayesFactor <- function(x, args = NULL) {
  # Create the analysis list
  analysis <- list()
  
  # Determine and set the method
  class <- class(x@numerator[[1]])[1]
  analysis$method <- dplyr::case_when(
    class == "BFoneSample" ~ "Bayesian t-test",
    class == "BFlinearModel" ~ "Bayesian linear regression",
    class == "BFcorrelation" ~ "Bayesian correlation",
    class == "BFcontingencyTable" ~ "Bayesian contingency table",
    class == "BFproportion" ~ "Bayesian analysis of proportions",
    class == "BFmetat" ~ "Bayesian meta-analysis"
  )
  
  # Extract bayes factors
  bayes_factors <- BayesFactor::extractBF(x)
  
  # Extract the statistics or loop over the models
  if (nrow(bayes_factors) == 1) {
    # Create a statistics list
    statistics <- list()
    
    # Extract statistics
    statistics <- add_statistic(statistics, "BF10", bayes_factors$bf, "BF", 
      "10")
    statistics <- add_statistic(statistics, "BF01", 1/bayes_factors$bf, "BF",
      "01")
    statistics <- add_statistic(statistics, "proportional error", 
      bayes_factors$error, "PE")
    
    # Add statistics to the analysis
    analysis$statistics <- statistics
  } else {
    # Create a list to store the different models in
    groups <- list(name = "Models")
    
    # Loop over the models
    for (i in 1:nrow(bayes_factors)) {
      # Create a list to store the model statistics in
      group <- list(name = rownames(bayes_factors)[i])
      
      # Create a list to add the statistics to
      statistics <- list()
      
      statistics <- add_statistic(statistics, "BF10", bayes_factors$bf[i], "BF",
        "10")
      statistics <- add_statistic(statistics, "BF01", 1/bayes_factors$bf[i], 
        "BF", "01")
      statistics <- add_statistic(statistics, "proportional error", 
        bayes_factors$error[i], "PE")
      
      # Add statistics to the model list
      group$statistics <- statistics
      
      # Add the model group to the groups list
      groups$groups <- append(groups$groups, list(group))
    }
    
    # Add the list of models to the analysis list
    analysis$groups <- append(analysis$groups, list(groups))
  }
  
  # Add denominator model information
  alternative <- list(
    name = x@denominator@longName,
    formula = x@denominator@identifier$formula
  )
  analysis$alternative <- alternative
  
  # Add package information
  analysis <- add_package_info(analysis, "BayesFactor")
  
  return(analysis)
}