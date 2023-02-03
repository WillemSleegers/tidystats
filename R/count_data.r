#' Count the number of observations
#'
#' `count_data` returns the number and percentage of observations for
#' categorical variables.
#'
#' @param data A data frame.
#' @param ... One or more unquoted (categorical) column names from the data
#'   frame, separated by commas.
#' @param na.rm A boolean specifying whether missing values (including NaN) 
#'   should be removed.
#'
#' @details The data frame can be grouped using [dplyr::group_by()]
#' so that the number of observations will be calculated within each group
#' level.
#'
#' @examples
#' count_data(quote_source, source)
#' count_data(quote_source, source, sex)
#' count_data(quote_source, source, sex, na.rm = TRUE)
#'
#' # Use dplyr::group_by() to get percentages within each group
#' quote_source |>
#'   dplyr::group_by(source) |>
#'   count_data(sex)
#'
#' @export
count_data <- function(data, ..., na.rm = FALSE) {
  if (!"data.frame" %in% class(data)) {
    stop("'data' is not a data frame.")
  }

  output <- dplyr::count(data, ...)

  # Remove missing observations if na.rm is set to TRUE
  if (na.rm) {
    output <- dplyr::filter(
      output,
      dplyr::if_all(dplyr::everything(), ~ !is.na(.))
    )
  }

  # Calculate percentage of each group per var
  output <- dplyr::mutate(output, pct = n / sum(n))

  # Add a tidystats class so we can use the tidy_stats() function to parse the
  # the output
  class(output) <- c("tidystats_counts", class(output))

  return(output)
}
