#' Calculate common descriptive statistics
#'
#' \code{describe_data} returns a set of common descriptive statistics
#' (e.g., n, mean, sd) for numeric variables.
#'
#' @param data A data frame.
#' @param ... One or more unquoted (numerical) column names from the data frame,
#' separated by commas.
#' @param na.rm Logical. Should missing values (including NaN) be excluded in
#' calculating the descriptives?
#'
#' @details The data set can be grouped using \strong{dplyr}'s \code{group_by}
#' so that descriptives will be calculated for each group level.
#'
#' Skew and kurtosis are based on the \code{skewness} and \code{kurtosis}
#' functions of the \strong{moments} package (Komsta & Novomestky, 2015).
#'
#' @examples
#' # Load the dplyr package for access to the %>% operator
#' library(dplyr)
#'
#' # 1 variable
#' describe_data(cox, avoidance)
#'
#' # 1 variable, 1 group
#' cox %>%
#'   group_by(condition) %>%
#'   describe_data(avoidance)
#'
#' # 2 variables
#' describe_data(cox, avoidance, anxiety)
#'
#' # 2 variables, 1 group
#' cox %>%
#'   group_by(condition) %>%
#'   describe_data(avoidance, anxiety)
#'
#' # 1 variable, 2 groups
#' cox %>%
#'   group_by(condition, sex) %>%
#'   describe_data(avoidance)
#'
#' # 2 variables, 2 groups
#' cox %>%
#'   group_by(condition, sex) %>%
#'   describe_data(avoidance, anxiety)
#'
#' @export
describe_data <- function(data, ..., na.rm = TRUE) {

  # Get variables
  vars <- dplyr::quos(...)
  var_names <- unlist(purrr::map(vars, rlang::as_name))

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
    data$variable <- as.character(vars[[1]])[2]
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
      skew     = (sum((value-mean(value, na.rm = na.rm))^3, na.rm = na.rm) /
                  N) / (sum((value - mean(value, na.rm = na.rm))^2,
                  na.rm = na.rm) / N)^(3 / 2),
      kurtosis = N * sum((value-mean(value, na.rm = na.rm))^4, na.rm = na.rm)/
                  (sum((value-mean(value, na.rm = na.rm))^2, na.rm = na.rm)^2)
    )
  
  # Group the output by the original grouping columns
  output <- dplyr::group_by_at(output, vars(grouping))
  
  # Sort the variable column by the argument order of the variables, rather than
  # alphabetically
  if (length(var_names) > 1) {
    output <- output %>%
      mutate(variable = factor(variable, levels = var_names)) %>%
      arrange(variable) %>%
      mutate(variable = as.character(variable))
  }
  
  # Add a tidystats class so we can use the tidy_stats() function to parse the
  # the output
  class(output) <- append(class(output), "tidystats_descriptives")

  return(output)
}