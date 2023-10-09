#' Convert a tidystats list to a data frame
#'
#' [tidy_stats_to_data_frame()] converts a tidystats list to a data frame,
#' which can then be used to extract specific statistics using standard
#' subsetting functions (e.g., [dplyr::filter()]).
#'
#' @param x A tidystats list.
#'
#' @examples
#' # Conduct analyses
#' sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)
#'
#' ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
#' trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
#' weight <- c(ctl, trt)
#' lm_D9 <- lm(weight ~ group)
#'
#' npk_aov <- aov(yield ~ block + N * P * K, npk)
#'
#' # Create an empty list to store the statistics in
#' statistics <- list()
#'
#' # Add statistics
#' statistics <- statistics |>
#'   add_stats(sleep_test, type = "primary", preregistered = TRUE) |>
#'   add_stats(lm_D9) |>
#'   add_stats(npk_aov, notes = "An ANOVA example")
#'
#' # Convert the list to a data frame
#' df <- tidy_stats_to_data_frame(statistics)
#'
#' # Select all the p-values
#' dplyr::filter(df, statistic_name == "p")
#'
#' @export
tidy_stats_to_data_frame <- function(x) {
  df <- purrr::map2_df(x, names(x), analysis_to_data_frame)

  df <- dplyr::rename(df, statistic_name = name)

  df <- dplyr::relocate(df, identifier, sort(tidyselect::peek_vars()))
  df <- dplyr::relocate(df, dplyr::any_of("symbol"), .after = statistic_name)
  df <- dplyr::relocate(df, dplyr::any_of("lower"), .before = value)
  df <- dplyr::relocate(df, dplyr::any_of("upper"), .after = value)
  df <- dplyr::relocate(
    df,
    dplyr::any_of(c("interval", "level")),
    .after = dplyr::last_col()
  )

  return(df)
}

analysis_to_data_frame <- function(x, y) {
  df <- tibble::tibble(identifier = y)

  if ("statistics" %in% names(x)) {
    df <- dplyr::bind_cols(
      df,
      purrr::map_df(x$statistics, function(x) {
        return(x)
      })
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

  if (!is.null(x$name)) {
    df <- dplyr::mutate(df, analysis_name = x$name, .after = identifier)
  }

  return(df)
}

groups_to_data_frame <- function(x, level) {
  level <- level + 1

  if ("statistics" %in% names(x)) {
    df <- purrr::map_df(x$statistics, function(x) {
      return(x)
    })
  }

  if ("groups" %in% names(x)) {
    df <- purrr::map_df(x$groups, groups_to_data_frame, level)
  }

  # Check if there's one or more names, if so, add them to the data frame and
  # append the level to the name
  if ("name" %in% names(x)) {
    df <- dplyr::mutate(df, "group_name_{level}" := as.character(x$name))
  }

  if ("names" %in% names(x)) {
    df <- dplyr::mutate(
      df,
      "group_name_{level}_1" := x$names[[1]]$name,
      "group_name_{level}_2" := x$names[[2]]$name,
    )
  }

  return(df)
}
