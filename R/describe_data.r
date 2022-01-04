#' Calculate common descriptive statistics
#'
#' \code{describe_data} returns a set of common descriptive statistics
#' (e.g., n, mean, sd) for numeric variables.
#'
#' @param data A data frame.
#' @param column An unquoted (numerical) column name from the data frame.
#' @param na.rm Logical. Should missing values (including NaN) be excluded in
#' calculating the descriptives? The default is TRUE.
#' @param short Logical. Should only a subset of descriptives be reported? If 
#' set to TRUE, only the N, M, and SD will be returned. The default is FALSE.
#'
#' @details The data can be grouped using \code{dplyr::group_by} so that 
#' descriptives will be calculated for each group level.
#' 
#' When na.rm is set to FALSE, a percentage column will be added to the output
#' that contains the percentage of non-missing data.
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
#' @importFrom dplyr %>%
#'
#' @export
describe_data <- function(data, column, na.rm = TRUE, short = FALSE) {

  # Check if 'data' is actually a data frame
  if (!"data.frame" %in% class(data)) {
    stop("'data' is not a data frame.")
  }
  
  # Check if the user specified a column
  if (missing(column)) {
    stop("No column found; please provide one.")
  }
  
  # Check whether the values in var are numeric
  if (sum(!class(dplyr::pull(data, {{ column }})) %in% 
      c("numeric", "integer")) > 0) {
    stop("The column does not contain numeric values.")
  }

  # Get grouping
  grouping <- dplyr::group_vars(data)

  # Select only the variable and grouping columns from the data
  data <- dplyr::select(data, dplyr::group_vars(data), {{ column }})
  
  # Calculate descriptives
  output <- data %>%
    dplyr::summarize(
      missing  = sum(is.na({{ column }})),
      N        = dplyr::n() - missing,
      M        = mean({{ column }}, na.rm = na.rm),
      SD       = sd({{ column }}, na.rm = na.rm),
      SE       = SD / sqrt(N),
      min      = min({{ column }}, na.rm = na.rm),
      max      = max({{ column }}, na.rm = na.rm),
      range    = diff(range({{ column }}, na.rm = na.rm)),
      median   = median({{ column }}, na.rm = na.rm),
      mode     = unique({{ column }})[which.max(tabulate(match({{ column }},
        unique({{ column }}))))],
      skew     = (sum(({{ column }} - mean({{ column }}, na.rm = na.rm))^3, 
        na.rm = na.rm) / N) / (sum(({{ column }} - 
            mean({{ column }}, na.rm = na.rm))^2, na.rm = na.rm) / 
            N)^(3 / 2),
      kurtosis = N * sum(({{ column }} - mean({{ column }}, na.rm = na.rm))^4, 
        na.rm = na.rm) / (sum(({{ column }} - mean({{ column }}, 
          na.rm = na.rm))^2, na.rm = na.rm)^2),
      .groups = "keep"
    )
  
  output <- dplyr::mutate(output, 
      var = dplyr::quo_name(dplyr::enquo(column))
    )
  # TODO: See if this can be done differently
  
  # Add percentage if na.rm = FALSE (if na.rm = TRUE it would always be 100)
  # Note that depending on the value of na.rm, we either ignore or include the
  # number of missing observations
  if (!na.rm) {
    output <- dplyr::mutate(output, pct = N / sum(N + missing) * 100)
  }
  
  # Reorder the columns and return only a subset if short was set to TRUE
  if (short) {
    output <- dplyr::select(output, var, dplyr::all_of(grouping), N, M, SD)
  } else {
    output <- dplyr::select(output, var, dplyr::all_of(grouping),
      missing, N, dplyr::contains("pct"), dplyr::everything())
  }
  
  # Add a tidystats class so we can use the tidy_stats() function to parse the
  # the output
  # Put it at the beginning otherwise we get an error when printing the tibble
  class(output) <- c("tidystats_descriptives", class(output))

  return(output)
}

