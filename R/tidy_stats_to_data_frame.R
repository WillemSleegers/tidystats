#' Convert a tidystats list to a data frame
#'
#' \code{tidy_stats_to_data_frame} converts a tidystats list to a data frame,
#' which can then be used to extract specific statistics using standard
#' subsetting functions (e.g., \code{dplyr::filter}).
#'
#' @param x A tidystats list.
#'
#' @examples
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
#' npk_aov <- aov(yield ~ block + N * P * K, npk)
#'
#' #' # Create an empty list
#' results <- list()
#'
#' # Add output to the results list
#' results <- results |>
#'   add_stats(sleep_test) |>
#'   add_stats(lm_D9, type = "primary", preregistered = TRUE) |>
#'   add_stats(npk_aov, notes = "An ANOVA example")
#'
#' # Convert the list to a data frame
#' results_df <- tidy_stats_to_data_frame(results)
#'
#' # Select all the p-values
#' dplyr::filter(results_df, statistic == "p")
#'
#' @export
tidy_stats_to_data_frame <- function(x) {
  # Loop over each analysis and convert each to a data frame
  output <- purrr::map2_dfr(x, names(x), analysis_to_data_frame)

  # Re-order output
  output <- dplyr::select_at(output, dplyr::vars(
    identifier, dplyr::contains("group"),
    dplyr::contains("term"), statistic, value, dplyr::contains("extra"), method,
    dplyr::everything()
  ))

  return(output)
}

analysis_to_data_frame <- function(x, y) {
  output <- tibble::tibble()

  if ("statistics" %in% names(x)) {
    output <- statistics_to_data_frame(x$statistics)
  }
  if ("terms" %in% names(x)) {
    output <- dplyr::bind_rows(output, terms_to_data_frame(x$terms))
  }
  if ("model" %in% names(x)) {
    output <- dplyr::bind_rows(
      output,
      statistics_to_data_frame(x$model$statistics)
    )
  }
  if ("groups" %in% names(x)) {
    output <- dplyr::bind_rows(output, groups_to_data_frame(x$groups))
  }
  if ("effects" %in% names(x)) {
    output <- dplyr::bind_rows(
      output,
      random_effects_to_data_frame(x$effects$random_effects),
      fixed_effects_to_data_frame(x$effects$fixed_effects)
    )
  }

  # Add identifier
  output$identifier <- y

  # Add the method
  output$method <- x$method

  # Add additional information, if present
  if ("type" %in% names(x)) {
    output$type <- x$type
  }
  if ("preregistered" %in% names(x)) {
    output$preregistered <- x$preregistered
  }

  return(output)
}

random_effects_to_data_frame <- function(x) {
  df_statistics <- statistics_to_data_frame(x$statistics)
  df_groups <- groups_to_data_frame(x$groups)

  return(dplyr::bind_rows(df_statistics, df_groups))
}

fixed_effects_to_data_frame <- function(x) {
  df_terms <- terms_to_data_frame(x$terms)
  df_pairs <- pairs_to_data_frame(x$pairs)

  return(dplyr::bind_rows(df_terms, df_pairs))
}

groups_to_data_frame <- function(x) {
  df <- purrr::map_df(x, group_to_data_frame)

  return(df)
}

group_to_data_frame <- function(x) {
  df <- tibble::tibble()

  if ("statistics" %in% names(x)) {
    df_statistics <- statistics_to_data_frame(x$statistics)
    df_statistics$group <- x$name
    df <- dplyr::bind_rows(df, df_statistics)
  }

  if ("terms" %in% names(x)) {
    df_terms <- terms_to_data_frame(x$terms)
    df_terms$group <- x$name
    df <- dplyr::bind_rows(df, df_terms)
  }

  if ("pairs" %in% names(x)) {
    df_pairs <- pairs_to_data_frame(x$pairs)
    df_pairs$group <- x$name
    df <- dplyr::bind_rows(df, df_pairs)
  }

  return(df)
}

terms_to_data_frame <- function(x) {
  df <- purrr::map_df(x, term_to_data_frame)

  return(df)
}

term_to_data_frame <- function(x) {
  df <- statistics_to_data_frame(x$statistics)
  df$term <- x$name

  return(df)
}

pairs_to_data_frame <- function(x) {
  df <- purrr::map_df(x, pair_to_data_frame)

  return(df)
}

pair_to_data_frame <- function(x) {
  df <- statistics_to_data_frame(x$statistics)
  df$term <- paste(x$names[[1]], "-", x$names[[2]])

  return(df)
}

statistics_to_data_frame <- function(x) {
  df <- purrr::map2_dfr(x, names(x), statistic_to_data_frame)

  return(df)
}

statistic_to_data_frame <- function(x, y) {
  if (inherits(x, "numeric") | inherits(x, "integer")) {
    df <- tibble::tibble(
      statistic = y,
      value = x
    )
  } else {
    if (y == "CI") {
      df <- CI_to_data_frame(x)
    } else if (y == "dfs") {
      df <- dfs_to_data_frame(x)
    } else if (y == "estimate" | y == "statistic") {
      df <- named_statistic_to_data_frame(x)
    }
  }

  return(df)
}

named_statistic_to_data_frame <- function(x) {
  return(
    tibble::tribble(
      ~"statistic", ~"value",
      x$name, x$value
    )
  )
}

dfs_to_data_frame <- function(x) {
  return(
    tibble::tribble(
      ~"statistic", ~"value",
      names(x)[1], x[[1]],
      names(x)[2], x[[2]],
    )
  )
}

CI_to_data_frame <- function(x) {
  return(
    tibble::tribble(
      ~"statistic", ~"value", ~"extra",
      "CI_lower", x$CI_lower, paste0(x$CI_level * 100, "% CI"),
      "CI_upper", x$CI_upper, paste0(x$CI_level * 100, "% CI"),
    )
  )
}
