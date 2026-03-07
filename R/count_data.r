#' Count the number of observations
#'
#' [count_data()] returns the number and proportion of observations for
#' categorical variables.
#'
#' @param data A data frame.
#' @param ... One or more unquoted (categorical) column names from the data
#'   frame, separated by commas.
#' @param by An optional character vector of column names to group by.
#' @param na.rm A boolean specifying whether missing values (including NaN)
#'   should be removed.
#'
#' @details Use the `by` argument to group the data, or alternatively pipe
#' grouped data created with [dplyr::group_by()].
#' @param pct A boolean indicating whether to calculate percentages instead of
#'   proportions. The default is `FALSE`.
#'
#' @examples
#' count_data(quote_source, source)
#' count_data(quote_source, source, sex)
#' count_data(quote_source, source, sex, na.rm = TRUE)
#' count_data(quote_source, source, sex, na.rm = TRUE, pct = TRUE)
#'
#' # Use the by argument to calculate proportions within a group
#' count_data(quote_source, sex, by = "source")
#'
#' @export
count_data <- function(data, ..., by = NULL, na.rm = FALSE, pct = FALSE) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }

  cols <- dots_to_names(...)
  existing_groups <- if (!is.null(by)) by else group_names(data)
  group_cols <- c(existing_groups, cols)

  sub <- data[, group_cols, drop = FALSE]

  # Remove missing observations if na.rm is set to TRUE
  if (na.rm) {
    sub <- sub[stats::complete.cases(sub), , drop = FALSE]
  }

  # Create row keys to identify unique combinations
  if (length(group_cols) == 0) {
    output <- data.frame(n = nrow(sub))
  } else {
    keys <- do.call(paste, c(lapply(sub, function(col) match(col, unique(col))), sep = ","))
    unique_keys <- unique(keys)
    output <- sub[match(unique_keys, keys), , drop = FALSE]
    rownames(output) <- NULL # Reset row names left over from subsetting
    output$n <- tabulate(match(keys, unique_keys))
  }

  # Calculate denominators: within each group if grouped, overall otherwise
  if (length(existing_groups) > 0) {
    group_keys <- do.call(paste, c(lapply(output[, existing_groups, drop = FALSE],
      function(col) match(col, unique(col))), sep = ","))
    totals <- as.numeric(tapply(output$n, group_keys, sum)[group_keys])
  } else {
    totals <- sum(output$n)
  }

  # Calculate proportion or percentage
  if (pct) {
    output$pct <- output$n / totals * 100
  } else {
    output$prop <- output$n / totals
  }

  # Add a tidystats class so we can use the tidy_stats() function to parse the
  # the output
  class(output) <- c("tidystats_counts", class(output))

  return(output)
}
