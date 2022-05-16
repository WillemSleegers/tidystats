#' Helper functions in tidystats
#'
#' @description Functions used under the hood in the \code{tidystats} package.
#'
#' @describeIn tidy_matrix
#' Function to convert matrix objects to a tidy data frame.
#'
#' @param m A matrix.

tidy_matrix <- function(m, symmetric = TRUE) {
  # Check whether there are row and column names
  if (!length(rownames(m)) > 0) {
    stop("Matrix has no row names.")
  }

  if (!length(colnames(m)) > 0) {
    stop("Matrix has no column names.")
  }

  # Check if the matrix is indeed symmetric
  if (symmetric) {
    if (sum(rownames(m) == colnames(m)) != length(rownames(m))) {
      stop("Matrix row and column names do not match.")
    }
  }

  # Remove the diagonal and duplicate values in case of a symmetric matrix
  if (symmetric) {
    m[lower.tri(m, diag = TRUE)] <- NA
  }

  # Tidy the matrix into a data frame
  df <- m %>%
    as.matrix() %>%
    tibble::as_tibble(rownames = "name1") %>%
    tidyr::pivot_longer(-name1, names_to = "name2", values_to = "value") %>%
    dplyr::filter(!is.na(value))

  return(df)
}

add_statistic <- function(list, name, value, symbol = NULL, subscript = NULL,
  interval = NULL, level = NULL, lower = NULL, upper = NULL) {

  if (!is.null(value)) {
    if (!is.na(value) || name == "p") {
      new_list <- list()
      new_list$name <- name

      if (!is.null(symbol)) {
        if (!is.na(symbol)) new_list$symbol <- symbol
      }

      if (!is.null(subscript)) {
        if (!is.na(subscript)) new_list$subscript <- subscript
      }

      new_list$value <- value

      if (!is.null(level)) {
        if (!is.na(level)) {
          new_list$interval <- interval
          new_list$level <- level
          new_list$lower <- lower
          new_list$upper <- upper
        }
      }

      list <- append(list, list(new_list))
    }
  }

  return(list)
}

add_package_info <- function(list, package) {
  list$package <- list(
    name = package,
    version = getNamespaceVersion(package)[[1]]
  )

  return(list)
}
