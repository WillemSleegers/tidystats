#' Calculate common descriptive statistics
#'
#' \code{describe_data} returns a set of common descriptive statistics
#' (e.g., n, mean, sd) for numeric variables.
#'
#' @param data A data frame.
#' @param ... One or more unquoted (numerical) column names from the data frame,
#' separated by commas.
#' @param na.rm Logical. Should missing values (including NaN) be excluded in
#' calculating the descriptives? The default is TRUE.
#' @param short Logical. Should only a subset of descriptives be reported? If 
#' set to TRUE, only the N, M, and SD will be returned. The default is FALSE.
#'
#' @details The data can be grouped using \code{dplyr::group_by} so that 
#' descriptives will be calculated for each group level.
#'
#' Skew and kurtosis are based on the \code{skewness} and \code{kurtosis}
#' functions of the \code{moments} package (Komsta & Novomestky, 2015).
#' 
#' Percentages are calculated based on the total of non-missing observations. 
#' When na.rm is set to FALSE, percentages are based on the total of missing and
#' non-missing observations.
#'
#' @examples
#' # Load the dplyr package for access to the %>% operator and group_by()
#' library(dplyr)
#'
#' # Inspect descriptives of the response column from the 'quote_source' data
#' # frame included in tidystats
#' describe_data(quote_source, response)
#'
#' # Repeat the former, now for each level of the source column
#' quote_source %>%
#'   group_by(source) %>%
#'   describe_data(response)
#'   
#' # Only inspect the total N, mean, and standard deviation
#' quote_source %>%
#'   group_by(source) %>%
#'   describe_data(response, short = TRUE)
#'
#' # Inspect multiple columns
#' describe_data(quote_source, response, age)
#'
#' @export
describe_data <- function(data, ..., na.rm = TRUE, short = FALSE) {

  # Get variables
  vars <- dplyr::quos(...)
  var_names <- unlist(purrr::map(vars, dplyr::quo_name))

  # Throw an error if no vars are supplied
  if (length(vars) == 0) {
    stop("No variables found")
  }

  # Check whether all variables are numeric
  if (sum(!sapply(data[, var_names],
      class) %in% c("numeric", "integer")) > 0) {
    stop("Not all variables are numeric.")
  }

  # Get grouping
  grouping <- dplyr::group_vars(data)

  # Select only the variables and grouping columns from the data
  data <- dplyr::select(data, dplyr::group_vars(data), !!! vars)

  # If there are multiple variables, restructure them into 'variable' and their
  # values into 'value'
  # Else add a 'variable' column and rename the one variable to 'value'
  if (length(vars) > 1) {
    data <- tidyr::gather(data, "variable", "value", !!! vars)
  } else {
    data$variable <- dplyr::quo_name(vars[[1]])
    data <- data %>%
      dplyr::ungroup() %>% # Temporary fix for a bug
      dplyr::rename_at(vars(!!! vars), ~"value")
  }

  # Re-group the data frame
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(vars(variable, grouping))

  # Calculate descriptives
  output <- data %>%
    dplyr::summarize(
      missing  = sum(is.na(value)),
      N        = dplyr::n() - missing,
      M        = mean(value, na.rm = na.rm),
      SD       = sd(value, na.rm = na.rm),
      SE       = SD / sqrt(N),
      min      = min(value, na.rm = na.rm),
      max      = max(value, na.rm = na.rm),
      range    = diff(range(value, na.rm = na.rm)),
      median   = median(value, na.rm = na.rm),
      mode     = unique(value)[which.max(tabulate(match(value,
                   unique(value))))],
      skew     = (sum((value - mean(value, na.rm = na.rm))^3, na.rm = na.rm) /
                  N) / (sum((value - mean(value, na.rm = na.rm))^2,
                  na.rm = na.rm) / N)^(3 / 2),
      kurtosis = N * sum((value - mean(value, na.rm = na.rm))^4, na.rm = na.rm)/
                  (sum((value - mean(value, na.rm = na.rm))^2, na.rm = na.rm)^2)
    )

  # Add percentage
  # Note that depending on the value of na.rm, we either ignore or include the
  # number of missing observations
  output <- dplyr::group_by(output, variable)
  if (na.rm) {
    output <- dplyr::mutate(output, pct = N / sum(N) * 100)
  } else {
    output <- dplyr::mutate(output, pct = N / sum(N + missing) * 100)
  }
    
  # Reorder the columns and return only a subset if short was set to TRUE
  if (short) {
    output <- dplyr::select(output, variable, grouping, N, M, SD)
  } else {
    output <- dplyr::select(output, variable, grouping, missing, N, pct, 
      dplyr::everything())
  }
  
  # Group the output by the original grouping columns
  output <- dplyr::group_by_at(output, vars(grouping))
  
  # Sort the variable column by the argument order of the variables, rather than
  # alphabetically
  if (length(var_names) > 1) {
    output <- output %>%
      dplyr::mutate(variable = factor(variable, levels = var_names)) %>%
      dplyr::arrange(variable) %>%
      dplyr::mutate(variable = as.character(variable))
  }
  
  # Add a tidystats class so we can use the tidy_stats() function to parse the
  # the output
  class(output) <- append(class(output), "tidystats_descriptives")

  return(output)
}