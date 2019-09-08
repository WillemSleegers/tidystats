#' Convert a tidystats list to a data frame
#'
#' \code{tidy_stats_to_data_frame} converts a tidystats list to a data frame, 
#' which can then be used to easily extract specific statistics using standard
#' subsetting functions (e.g., \code{dplyr::filter}).
#'
#' @param x A tidystats list.
#' 
#' @examples
#' # Load dplyr for access to the piping operator
#' library(dplyr)
#'   
#' # Conduct statistical tests
#' t_test_1 <- t.test(1:10, y = c(7:20))
#' t_test_2 <- t.test(1:10, y = c(7:20, 200))
#' t_test_3 <- t.test(extra ~ group, data = sleep)
#' 
#' #' # Create an empty list
#' results <- list()
#' 
#' # Add tests to the empty list
#' results <- results %>%
#'   add_stats(t_test_1) %>%
#'   add_stats(t_test_2) %>%
#'   add_stats(t_test_3)
#'   
#' # Convert the list to a data frame
#' results_df <- tidy_stats_to_data_frame(results)
#' 
#' # Select all the p-values
#' results_df %>%
#'   filter(statistic == "p") %>%
#'   pull(value)
#'
#' @export

#TODO: Simply the methods (e.g., get rid of simulated p value description)

tidy_stats_to_data_frame <- function(x) {

  # Create a vector with names of each list element (which are the identifiers)
  identifiers <- names(x)

  # Loop over each analysis and convert each to a data frame
  output <- purrr::map_df(x, analysis_to_data_frame, .id = "identifier")
  
  # Re-order output
  output <- dplyr::select_at(output, dplyr::vars(identifier, method, 
    dplyr::contains("DV"), dplyr::contains("group"), dplyr::contains("term"), 
    dplyr::everything()))
  
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
  
  # Add additional information, if present
  if ("type" %in% names(x)) {
    output$type <- x$type
  }
  if ("preregistered" %in% names(x)) {
    output$preregistered <- x$preregistered
  }
  
  return(output)
}

descriptives_to_data_frame <- function(x) {
  # Loop over the variables and possibly the groups, and extract the statistics
  df <- purrr::map_df(x$variables, function(x) {
    if ("groups" %in% names(x)) {
        df <- purrr::map_df(x$groups, function(x) {
        df <- purrr::map2_df(x$statistics, names(x$statistics), 
          statistics_to_data_frame)
        df$group <- x$name
      
        return(df)
      })
    } else {
      df <- purrr::map2_df(x$statistics, names(x$statistics), 
        statistics_to_data_frame)
    }
    
    df$DV <- x$name
    
    return(df)
  })

  # Re-order the columns
  df <- dplyr::select_at(df, dplyr::vars(DV, dplyr::contains("group"), 
    dplyr::everything()))

  return(df)
}

lm_to_data_frame <- function(x) {
  # Convert the coefficient-related statistics to a data frame
  coefficients <- purrr::map_df(x$coefficients, function(x) {
    coefficient <- x$name
    statistics <- purrr::map2_df(x$statistics, names(x$statistics), 
      statistics_to_data_frame)
    output <- dplyr::mutate(statistics, term = coefficient)

    return(output)
  })
  
  # Add coefficients
  coefficients <- dplyr::mutate(coefficients, group = "coefficients")
  
  # Convert the model fit statistics to a data frame
  model <- purrr::map2_df(x$model$statistics, names(x$model$statistics),
    statistics_to_data_frame)
  model <- dplyr::mutate(model, group = "model", )
  
  # Combine both the coefficients and model data frames
  output <- dplyr::bind_rows(coefficients, model)
  
  # Re-order columns
  output <- dplyr::select(output, group, term, dplyr::everything())
  
  return(output)
}

aov_to_data_frame <- function(x) {
  
  # Check whether the ANOVA has a within-subjects factor
  if ("groups" %in% names(x)) {
      output <- purrr::map_df(x$groups, function(x) {
        group <- stringr::str_remove(x$name, "Error: ")
        coefficients <- purrr::map_df(x$coefficients, function(x) {
          coefficient <- x$name
          statistics <- purrr::map2_df(x$statistics, names(x$statistics),
          statistics_to_data_frame)
          
          output <- dplyr::mutate(statistics, term = coefficient)

          return(output)
        })

        output <- dplyr::mutate(coefficients, group = group)

        return(output)
      })
    } else {
      output <- purrr::map_df(x$coefficients, function(x) {
        coefficient <- x$name
        statistics <- purrr::map2_df(x$statistics, names(x$statistics), 
          statistics_to_data_frame)
        output <- dplyr::mutate(statistics, term = coefficient)

        return(output)
      })
    }
  
  return(output)
}

htest_to_data_frame <- function(x) {
  statistics <- x$statistics
    statistics_df <- purrr::map2_df(statistics, names(statistics),
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
    tibble::tibble(
      statistic = name,
      value = x
    )
  )
}

statistic_to_data_frame <- function(x) {
  return(
    tibble::tibble(
      statistic = "statistic",
      value = x$value,
      extra = paste(x$name, "statistic", sep = "-")
    )
  )
}

dfs_to_data_frame <- function(x) {
  return(
    tibble::tribble(
      ~"statistic", ~"value",
      "numerator df", x$numerator_df,
      "denominator df", x$denominator_df,
    )
  )
}

CI_to_data_frame <- function(x) {
  return(
    tibble::tribble(
      ~"statistic", ~"value", ~"extra",
      "CI lower", x$lower, paste0(x$level * 100, "% CI"),
      "CI upper", x$upper, paste0(x$level * 100, "% CI"),
    )
  )
}

