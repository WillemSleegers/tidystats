#' Convert a tidystats list to a data frame
#'
#' \code{tidy_stats_to_data_frame} converts a tidystats list to a data frame,
#' which can then be used to extract specific statistics using standard
#' subsetting functions (e.g., \code{dplyr::filter()}).
#'
#' @param x A tidystats list.
#'
#' @examples
#' # Load dplyr for access to the piping operator
#' library(dplyr)
#'
# Conduct statistical tests
#' # t-test:
#' sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)
#'
#' # lm:
#' ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
#' trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
#' weight <- c(ctl, trt)
#' lm_D9 <- lm(weight ~ group)
#'
#' # ANOVA:
#' npk_aov <- aov(yield ~ block + N*P*K, npk)
#'
#' #' # Create an empty list
#' results <- list()
#'
#' # Add output to the results list
#' results <- results %>%
#'   add_stats(sleep_test) %>%
#'   add_stats(lm_D9, type = "primary", preregistered = TRUE) %>%
#'   add_stats(npk_aov, notes = "An ANOVA example")
#'
#' # Convert the list to a data frame
#' results_df <- tidy_stats_to_data_frame(results)
#'
#' # Select all the p-values
#' filter(results_df, statistic == "p")
#'
#' @export
tidy_stats_to_data_frame <- function(x) {
  # Convert statistics to a data frame
  df <- purrr::map2_df(x, names(x), analysis_to_data_frame)

  # Rename name to statistic_name
  df <- dplyr::rename(df, statistic_name = name)

  # Reorder the columns
  df <- dplyr::relocate(df, identifier, sort(tidyselect::peek_vars()))
  df <- dplyr::relocate(df, dplyr::any_of("symbol"), .after = statistic_name)
  df <- dplyr::relocate(df, dplyr::any_of("lower"), .before = value)
  df <- dplyr::relocate(df, dplyr::any_of("upper"), .after = value)

  return(df)
}

analysis_to_data_frame <- function(x, y) {
  # Create a data frame with the identifier of the analysis
  df <- tibble::tibble(identifier = y)

  # Check if there is a name for the analysis, if so, add it to the data frame
  if ("name" %in% names(x)) {
    df <- dplyr::mutate(df, analysis_name = x$name)
  }

  # Check if there are statistics, if so, convert them to a data frame and add
  # them to the data frame of the analysis
  if ("statistics" %in% names(x)) {
    df <- dplyr::bind_cols(
      df,
      purrr::map_df(x$statistics, function(x)
        return(ifelse(x == "-", as.numeric(NA), x)))
    )
  }

  # Check if there are groups, if so, recursively loop through them and convert
  # each group to a data frame, with a level integer to keep track of each
  # group name
  if ("groups" %in% names(x)) {
    level <- 0
    df <- dplyr::bind_cols(
      df,
      purrr::map_df(x$groups, groups_to_data_frame, level)
    )
  }

  return(df)
}

groups_to_data_frame <- function(x, level) {
  # Increment the level
  level <- level + 1

  # Check if there are statistics, if so, convert them to a data frame
  if ("statistics" %in% names(x)) {
    df <-
      purrr::map_df(x$statistics, function(x)
        return(ifelse(x == "-", as.numeric(NA), x)))
  }
  # Check if there are groups, if so, convert them to a data frame (recursively)
  if ("groups" %in% names(x)) {
    df <- purrr::map_df(x$groups, groups_to_data_frame, level)
  }

  # Check if there's a name, if so, add it to the data frame and append the
  # level to the name
  if ("name" %in% names(x)) {
    df <- dplyr::mutate(df, "group_name{level}" := x$name)
  }

  return(df)
}
