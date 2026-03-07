#' Calculate common descriptive statistics
#'
#' [describe_data()] returns a set of common descriptive statistics
#' (e.g., number of observations, mean, standard deviation) for one or more
#' numeric variables.
#'
#' @param data A data frame.
#' @param ... One or more unquoted column names from the data frame.
#' @param by An optional character vector of column names to group by.
#' @param na.rm A boolean indicating whether missing values (including NaN)
#'   should be excluded in calculating the descriptives? The default is TRUE.
#'
#' @details Use the `by` argument to group the data, or alternatively pipe
#' grouped data created with [dplyr::group_by()].
#' @param short A boolean indicating whether only a subset of descriptives
#'   should be reported? If set to `TRUE``, only the N, M, and SD will be
#'   returned. The default is `FALSE`.
#'
#' @examples
#' describe_data(quote_source, response)
#'
#' describe_data(quote_source, response, na.rm = FALSE)
#'
#' describe_data(quote_source, response, by = "source")
#'
#' describe_data(quote_source, response, by = "source", short = TRUE)
#'
#' @export
describe_data <- function(data, ..., by = NULL, na.rm = TRUE, short = FALSE) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.")

  # Check if the user provided any columns.
  if (...length() == 0) {
    stop("No columns found; please provide one or more columns.")
  }

  # Check if the columns exist in the data frame.
  column_names <- dots_to_names(...)
  if (sum(!column_names %in% names(data)) != 0) {
    stop(
      paste(
        "Did not find the following column(s) in the data:",
        paste(column_names[!column_names %in% names(data)], collapse = ", ")
      )
    )
  }

  # Check whether the values in the columns are numeric
  data_plain <- as.data.frame(data)
  columns <- data_plain[, column_names, drop = FALSE]
  column_classes <- vapply(columns, class, character(1))
  if (sum(!column_classes %in% c("numeric", "integer")) > 0) {
    stop(
      paste(
        "The following columns are not numeric:",
        paste(column_names[!column_classes %in% c("numeric", "integer")])
      )
    )
  }

  grouping <- if (!is.null(by)) by else group_names(data)

  # Select only the relevant columns
  data_plain <- data_plain[, c(grouping, column_names), drop = FALSE]

  # Pivot longer: reshape from wide to long
  if (length(grouping) > 0) {
    rows <- lapply(column_names, function(col) {
      cbind(
        data_plain[, grouping, drop = FALSE],
        data.frame(var = col, value = data_plain[[col]], stringsAsFactors = FALSE)
      )
    })
  } else {
    rows <- lapply(column_names, function(col) {
      data.frame(var = col, value = data_plain[[col]], stringsAsFactors = FALSE)
    })
  }
  data_long <- do.call(rbind, rows)
  rownames(data_long) <- NULL

  # Create grouping keys for aggregation (grouping cols + var)
  all_groups <- c(grouping, "var")
  keys <- do.call(paste, c(lapply(data_long[, all_groups, drop = FALSE], function(col) match(col, unique(col))), sep = ","))
  unique_keys <- unique(keys)

  # Calculate descriptives for each unique group-var combination
  results <- lapply(unique_keys, function(k) {
    sub <- data_long[keys == k, ]
    vals <- sub$value
    group_info <- sub[1, all_groups, drop = FALSE]

    n_total <- length(vals)
    missing_n <- sum(is.na(vals))
    N <- n_total - missing_n
    M <- mean(vals, na.rm = na.rm)
    SD <- sd(vals, na.rm = na.rm)
    SE <- SD / sqrt(N)
    minv <- min(vals, na.rm = na.rm)
    maxv <- max(vals, na.rm = na.rm)
    rangev <- diff(range(vals, na.rm = na.rm))
    medianv <- median(vals, na.rm = na.rm)

    vals_for_mode <- if (na.rm) vals[!is.na(vals)] else vals
    modev <- unique(vals_for_mode)[which.max(tabulate(match(
      vals_for_mode, unique(vals_for_mode)
    )))]

    mean_v <- mean(vals, na.rm = na.rm)
    deviations <- vals - mean_v
    skewv <- (sum(deviations^3, na.rm = na.rm) / N) /
      (sum(deviations^2, na.rm = na.rm) / N)^(3 / 2)
    kurtosisv <- N * sum(deviations^4, na.rm = na.rm) /
      (sum(deviations^2, na.rm = na.rm)^2)

    cbind(
      group_info,
      data.frame(
        missing = missing_n, N = N, M = M, SD = SD, SE = SE,
        min = minv, max = maxv, range = rangev, median = medianv,
        mode = modev, skew = skewv, kurtosis = kurtosisv,
        stringsAsFactors = FALSE
      )
    )
  })

  output <- do.call(rbind, results)
  rownames(output) <- NULL

  # Reorder the columns and return only a subset if short was set to TRUE
  if (short) {
    output <- output[, c("var", grouping, "N", "M", "SD"), drop = FALSE]
  } else {
    output <- output[, c("var", grouping, setdiff(names(output), c("var", grouping))),
      drop = FALSE
    ]
  }

  output <- output[order(output$var), ]
  rownames(output) <- NULL

  # Preserve grouping information for tidy_stats() to read
  if (length(grouping) > 0) {
    attr(output, "groups") <- structure(
      vector("list", length(grouping) + 1),
      names = c(grouping, ".rows")
    )
  }

  # Add a tidystats class so we can use the tidy_stats() function to parse the
  # the output
  class(output) <- c("tidystats_descriptives", class(output))

  return(output)
}
