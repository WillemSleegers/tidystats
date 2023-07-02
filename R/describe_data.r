#' Calculate common descriptive statistics
#'
#' [describe_data()] returns a set of common descriptive statistics
#' (e.g., number of observations, mean, standard deviation) for one or more
#' numeric variables.
#'
#' @param data A data frame.
#' @param ... One or more unquoted column names from the data frame.
#' @param na.rm A boolean indicating whether missing values (including NaN)
#'   should be excluded in calculating the descriptives? The default is TRUE.
#' @param short A boolean indicating whether only a subset of descriptives
#'   should be reported? If set to `TRUE``, only the N, M, and SD will be
#'   returned. The default is `FALSE`.
#'
#' @details The data can be grouped using [dplyr::group_by()] so that
#' descriptives will be calculated for each group level.
#'
#' Skew and kurtosis are based on the [skewness()] and [kurtosis()] functions
#' (Komsta & Novomestky, 2015).
#'
#' @examples
#' describe_data(quote_source, response)
#'
#' describe_data(quote_source, response, na.rm = FALSE)
#'
#' quote_source |>
#'   dplyr::group_by(source) |>
#'   describe_data(response)
#'
#' quote_source |>
#'   dplyr::group_by(source) |>
#'   describe_data(response, short = TRUE)
#'
#' @export
describe_data <- function(
    data, ..., na.rm = TRUE, short = FALSE) {
  # Check if 'data' is actually a data frame
  if (!"data.frame" %in% class(data)) {
    stop("'data' is not a data frame.")
  }

  # Check if the user provided any columns.
  if (length(rlang::enquos(...)) == 0) {
    stop("No columns found; please provide one or more columns.")
  }

  # Check if the columns exist in the data frame.
  column_names <- as.character(rlang::exprs(...))
  if (sum(!column_names %in% names(data)) != 0) {
    stop(
      paste(
        "Did not find the following column(s) in the data:",
        paste(column_names[!column_names %in% names(data)], collapse = ", ")
      )
    )
  }

  # Check whether the values in the columns are numeric
  columns <- dplyr::select(dplyr::ungroup(data), ...)
  if (sum(!purrr::map_chr(columns, class) %in% c("numeric", "integer")) > 0) {
    stop(
      paste(
        "The following columns are not numeric:",
        paste(column_names[!purrr::map_chr(columns, class) %in%
          c("numeric", "integer")])
      )
    )
  }

  grouping <- dplyr::group_vars(data)

  data <- dplyr::select(data, dplyr::all_of(grouping), ...)

  data <- tidyr::pivot_longer(data,
    cols = -dplyr::all_of(grouping),
    names_to = "var"
  )

  # Add the var to the existing grouping
  data <- dplyr::group_by(data, var, .add = TRUE)

  # Calculate descriptives
  output <- data %>%
    dplyr::summarize(
      missing = sum(is.na(value)),
      N = dplyr::n() - missing,
      M = mean(value, na.rm = na.rm),
      SD = sd(value, na.rm = na.rm),
      SE = SD / sqrt(N),
      min = min(value, na.rm = na.rm),
      max = max(value, na.rm = na.rm),
      range = diff(range(value, na.rm = na.rm)),
      median = median(value, na.rm = na.rm),
      mode = unique(value)[which.max(tabulate(match(
        value,
        unique(value)
      )))],
      skew = (sum((value - mean(value, na.rm = na.rm))^3,
        na.rm = na.rm
      ) / N) / (sum((value - mean(value, na.rm = na.rm))^2,
        na.rm = na.rm
      ) / N)^(3 / 2),
      kurtosis = N * sum((value - mean(value, na.rm = na.rm))^4,
        na.rm = na.rm
      ) / (sum((value - mean(value, na.rm = na.rm))^2,
        na.rm = na.rm
      )^2)
    )

  # Reorder the columns and return only a subset if short was set to TRUE
  if (short) {
    output <- dplyr::select(
      output,
      dplyr::all_of(c("var", grouping, "N", "M", "SD"))
    )
  } else {
    output <- dplyr::relocate(output, var, dplyr::all_of(grouping))
  }

  output <- dplyr::arrange(output, var)

  # Add a tidystats class so we can use the tidy_stats() function to parse the
  # the output
  # Put it at the beginning otherwise we get an error when printing the tibble
  class(output) <- c("tidystats_descriptives", class(output))

  return(output)
}
