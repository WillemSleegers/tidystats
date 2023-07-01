#' Helper functions in tidystats
#'
#' Functions used under the hood in the [tidystats] package.

#' @describeIn helper_functions
#' Function to convert matrix objects to a tidy data frame.
#'
#' @param m A matrix.
#'
#' @keywords internal

tidy_matrix <- function(m, symmetric = TRUE) {
  if (!length(rownames(m)) > 0) {
    stop("Matrix has no row names.")
  }

  if (!length(colnames(m)) > 0) {
    stop("Matrix has no column names.")
  }

  if (symmetric) {
    if (sum(rownames(m) == colnames(m)) != length(rownames(m))) {
      stop("Matrix row and column names do not match.")
    }
  }

  # Remove the diagonal and duplicate values in case of a symmetric matrix
  if (symmetric) {
    m[lower.tri(m, diag = TRUE)] <- NA
  }

  df <- m %>%
    as.matrix() %>%
    tibble::as_tibble(rownames = "name1") %>%
    tidyr::pivot_longer(-name1, names_to = "name2", values_to = "value") %>%
    dplyr::filter(!is.na(value))

  return(df)
}

# Tidying -----------------------------------------------------------------

#' @describeIn helper_functions
#' Function to add a statistic to list. It helps create the list and ignores
#' NULL values.
#'
#' @keywords internal

add_statistic <- function(list, name, value, symbol = NULL, subscript = NULL,
                          interval = NULL, level = NULL, lower = NULL,
                          upper = NULL) {
  if (!is.null(value)) {
    if (!is.na(value)) {
      new_list <- list()
      new_list$name <- name

      if (!is.null(symbol)) {
        if (!is.na(symbol)) new_list$symbol <- symbol
      }

      if (!is.null(subscript)) {
        if (!is.na(subscript)) new_list$subscript <- subscript
      }

      new_list$value <- value

      if (
        !is.null(level) &&
          !is.null(interval) &&
          !is.null(lower) &&
          !is.null(upper)
      ) {
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

add_attribute <- function(list, object, attribute) {
  value <- attr(object, attribute)
  if (!is.null(value)) {
    list[attribute] <- value
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

# Testing -----------------------------------------------------------------

#' @describeIn helper_functions
#' Function to compare tidied models during testing.
#'
#' @keywords internal

expect_equal_models <- function(model, expected_tidy_model, tolerance = 0.001) {
  # Convert model output to a tidystats list
  tidy_model <- tidy_stats(model)

  # Set package information to NULL because this may have changed since the
  # data was last saved
  tidy_model$package <- NULL
  expected_tidy_model$package <- NULL

  # Test whether the two lists are equal
  testthat::expect_equal(tidy_model, expected_tidy_model, tolerance = tolerance)
}

#' @describeIn helper_functions
#' Function to save tidied statistics to a file. Since these files are used
#' during testing, it's important to only store files with correctly tidied
#' statistics, hence the prompt.
#'
#' @keywords internal

write_test_stats <- function(x, path, digits = 6) {
  choice <- menu(
    title = "Are you sure you want to save these (test) statistics?",
    choices = c("Yes", "No")
  )

  if (choice == 1) {
    write_stats(x = x, path = path, digits = digits)
  }
}
