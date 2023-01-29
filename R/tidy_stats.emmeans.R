#' @describeIn tidy_stats tidy_stats method for class 'emmGrid'
#' @export
tidy_stats.emmGrid <- function(x, args = NULL) {
  return(tidy_stats(summary(x)))
}

#' @describeIn tidy_stats tidy_stats method for class 'summary_emm'
#' @export
tidy_stats.summary_emm <- function(x, args = NULL) {
  analysis <- list()

  analysis$method <- dplyr::case_when(
    "contrast" %in% names(x) ~ "Contrasts",
    TRUE ~ "Estimated marginal means"
  )

  df <- as.data.frame(x)

  by_vars <- attr(x, "by.vars")
  pri_vars <- attr(x, "pri.vars")
  vars <- c(by_vars, pri_vars)

  cl_names <- attr(x, "clNames")
  mesg <- attr(x, "mesg")
  cl_level <- mesg |>
    paste(mesg, collapse = "") |>
    stringr::str_extract("(?<=Confidence level used: )0.[0-9]+") |>
    as.numeric()

  group_statistics <- function(vars, df) {
    var <- vars[1]
    group <- list(name = var)

    levels <- unique(df[, var])

    for (level in levels) {
      group_level <- list(name = level)
      group_df <- df[df[, var] == level, ]

      if (length(vars) > 1) {
        group_level$groups <- append(
          group_level$groups,
          list(group_statistics(vars[-1], group_df))
        )
      } else {
        statistics <- list() |>
          add_statistic(
            "EMM",
            group_df$emmean,
            interval = "CI",
            level = cl_level,
            lower = group_df[, cl_names[1]],
            upper = group_df[, cl_names[2]]
          ) |>
          add_statistic(
            "estimate",
            group_df$estimate,
            symbol = "b"
          ) |>
          add_statistic(
            "estimate",
            group_df$response,
            symbol = "b"
          ) |>
          add_statistic(
            "estimate",
            group_df$T.square,
            symbol = "t²"
          ) |>
          add_statistic("SE", group_df$SE) |>
          add_statistic("statistic", group_df$t.ratio, symbol = "t") |>
          add_statistic("statistic", group_df$F.ratio, symbol = "F") |>
          add_statistic("df", group_df$df) |>
          add_statistic(
            "df numerator", group_df$df1,
            symbol = "df", subscript = "num."
          ) |>
          add_statistic(
            "df denominator", group_df$df2,
            symbol = "df", subscript = "den."
          ) |>
          add_statistic("df", group_df$df) |>
          add_statistic("p", group_df$p.value)

        group_level$statistics <- statistics
      }

      group$groups <- append(group$groups, list(group_level))
    }

    return(group)
  }

  if (!is.null(vars)) {
    analysis$groups <- append(analysis$groups, list(group_statistics(vars, df)))
  } else {
    analysis$statistics <- list() |>
      add_statistic("statistic", df$F.ratio, symbol = "F") |>
      add_statistic(
        "df numerator", df$df1,
        symbol = "df", subscript = "num."
      ) |>
      add_statistic(
        "df denominator", df$df2,
        symbol = "df", subscript = "den."
      ) |>
      add_statistic("p", df$p.value)
  }

  analysis <- add_attribute(analysis, x, "adjust")
  analysis <- add_attribute(analysis, x, "type")
  analysis <- add_attribute(analysis, x, "linkname")

  analysis <- add_package_info(analysis, "emmeans")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'emm_list'
#' @export
tidy_stats.emm_list <- function(x, args = NULL) {
  analysis <- list()

  names <- names(x)

  for (name in names) {
    group <- list(
      name = dplyr::case_when(
        name == "emmeans" ~ "Estimated marginal means",
        name == "contrasts" ~ "Contrasts",
        TRUE ~ name
      )
    )

    group <- append(group, tidy_stats.emmGrid(x[[name]]))

    # Remove package information because we need to set it only once here and
    # not for every group
    group$package <- NULL

    analysis$groups <- append(analysis$groups, list(group))
  }

  analysis <- add_package_info(analysis, "emmeans")

  return(analysis)
}
