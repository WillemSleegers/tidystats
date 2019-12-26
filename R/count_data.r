#' Count the number of observations
#'
#' \code{count_data} returns the number of observations for categorical
#' variables.
#'
#' @param data A data frame.
#' @param ... One or more unquoted (categorical) column names from the data
#' frame, separated by commas.
#' @param na.rm Logical. Should missing values (including NaN) be removed?
#'
#' @details The data frame can be grouped using \strong{dplyr}'s \code{group_by}
#' so that the number of observations will be calculated for each group level.
#'
#' @examples
#' # Load dplyr for access to the %>% operator and group_by()
#' library(dplyr)
#'
#' # 1 variable
#' count_data(quote_source, source)
#'
#' # 2 variables
#' count_data(quote_source, source, sex)
#' 
#' # Ignore missing values
#' count_data(quote_source, source, sex, na.rm = TRUE)
#' 
#' # Use group_by() to get percentages within each group
#' quote_source %>%
#'   group_by(source) %>%
#'   count_data(sex)
#' 
#' @export
count_data <- function(data, ..., na.rm = FALSE) {

  # Check if 'data' is actually a data frame
  if (!"data.frame" %in% class(data)) {
    stop("'data' is not a data frame.")
  }
  
  # Get variables
  vars <- quos(...)
  
  # Check if the user supplied any columns
  if (length(vars) == 0) {
    stop("No columns specified.")
  }

  # Calculate descriptives
  output <- dplyr::count(data, !!! vars)
  
  # Remove missing observations if na.rm is set to TRUE
  if (na.rm) {
    output <- filter_all(output, all_vars(!is.na(.)))
  }

  # Calculate percentage of each group per var
  output <- output %>%
    dplyr::mutate(pct = n / sum(n) * 100)
  
  # Add a tidystats class so we can use the tidy_stats() function to parse the
  # the output
  class(output) <- append(class(output), "tidystats_counts")
  
  return(output)
}