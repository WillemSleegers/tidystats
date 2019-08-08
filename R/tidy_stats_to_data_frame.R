#' Convert a tidy stats list to a data frame
#'
#' \code{tidy_stats_to_data_frame} converts a tidy stats list to a data frame.
#'
#' @param results A tidy stats list.
#'
#' @export

#TODO: Simply the methods (e.g., get rid of simulated p value description)

tidy_stats_to_data_frame <- function(x) {

  # Create a vector with names of each list element (which are the identifiers)
  identifiers <- names(x)

  # Loop over each analysis and convert each to a data frame
  output <- map_df(x, analysis_to_data_frame, .id = "identifier")
  
  # Re-order output
  output <- select_at(output, vars(identifier, method, contains("DV"), 
    contains("group"), contains("term"), everything()))
  
  return(output)
}

analysis_to_data_frame <- function(x) {

  # Extract the method information
  method <- x$method

  # Extract the statistics
  # Depending on the method used, group the statistics in a group column
  if (method == "Linear regression") {
    output <- lm_to_data_frame(x)
  } else if (method == "ANOVA") {
    output <- aov_to_data_frame(x)
  } else if (method == "Descriptives") {
    output <- descriptives_to_data_frame(x)
  } else {
    output <- htest_to_data_frame(x)
  }

  # Add the method
  output$method <- method
  
  return(output)
}

descriptives_to_data_frame <- function(x) {
  # Loop over the variables and possibly the groups, and extract the statistics
  df <- map_df(x$variables, function(x) {
    if ("groups" %in% names(x)) {
        df <- map_df(x$groups, function(x) {
        df <- map2_df(x$statistics, names(x$statistics), statistics_to_data_frame)
        df$group <- x$name
      
        return(df)
      })
    } else {
      df <- map2_df(x$statistics, names(x$statistics), statistics_to_data_frame)
    }
    
    df$DV <- x$name
    
    return(df)
  })

  # Re-order the columns
  df <- select_at(df, vars(DV, contains("group"), everything()))

  return(df)
}

lm_to_data_frame <- function(x) {
  # Convert the coefficient-related statistics to a data frame
  coefficients <- map_df(x$coefficients, function(x) {
    coefficient <- x$name
    statistics <- map2_df(x$statistics, names(x$statistics), 
      statistics_to_data_frame)
    output <- mutate(statistics, term = coefficient)

    return(output)
  })
  
  # Add
  coefficients <- mutate(coefficients, group = "coefficients")
  
  # Convert the model fit statistics to a data frame
  model <- map2_df(x$model$statistics, names(x$model$statistics),
    statistics_to_data_frame)
  model <- mutate(model, group = "model", )
  
  # Combine both the coefficients and model data frames
  output <- bind_rows(coefficients, model)
  
  # Re-order columns
  output <- select(output, group, term, everything())
  
  return(output)
}

aov_to_data_frame <- function(x) {
  
  # Check whether the ANOVA has a within-subjects factor
  if ("groups" %in% names(x)) {
      output <- map_df(x$groups, function(x) {
        group <- str_remove(x$name, "Error: ")
        coefficients <- map_df(x$coefficients, function(x) {
          coefficient <- x$name
          statistics <- map2_df(x$statistics, names(x$statistics),
          statistics_to_data_frame)
          
          output <- mutate(statistics, term = coefficient)

          return(output)
        })

        output <- mutate(coefficients, group = group)

        return(output)
      })
    } else {
      output <- map_df(x$coefficients, function(x) {
        coefficient <- x$name
        statistics <- map2_df(x$statistics, names(x$statistics), 
          statistics_to_data_frame)
        output <- mutate(statistics, term = coefficient)

        return(output)
      })
    }
  
  return(output)
}

htest_to_data_frame <- function(x) {
  statistics <- x$statistics
    statistics_df <- map2_df(statistics, names(statistics),
      statistics_to_data_frame)
    output <- statistics_df
    
  return(output)
}

statistics_to_data_frame <- function(x, name) {
  if (name == "statistic") {
    output <- statistic_to_data_frame(x)
  } else if (name == "CI") {
    output <- CI_to_data_frame(x)
  } else if (name == "dfs") {
    output <- dfs_to_data_frame(x)
  } else {
    output <- value_to_data_frame(x, name)
  }

  return(output)
}

value_to_data_frame <- function(x, name) {
  return(
    tibble(
      statistic = name,
      value = x
    )
  )
}

statistic_to_data_frame <- function(x) {
  return(
    tibble(
      statistic = "statistic",
      value = x$value,
      extra = paste(x$name, "statistic", sep = "-")
    )
  )
}

dfs_to_data_frame <- function(x) {
  return(
    tribble(
      ~"statistic", ~"value",
      "numerator df", x$numerator_df,
      "denominator df", x$denominator_df,
    )
  )
}

CI_to_data_frame <- function(x) {
  return(
    tribble(
      ~"statistic", ~"value", ~"extra",
      "CI lower", x$lower, paste0(x$level * 100, "% CI"),
      "CI upper", x$upper, paste0(x$level * 100, "% CI"),
    )
  )
}

