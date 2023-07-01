#' @describeIn tidy_stats tidy_stats method for class 'emmGrid'
#' @keywords internal
tidy_stats.emmGrid <- function(x, args = NULL) {
  return(tidy_stats(summary(x)))
}

#' @describeIn tidy_stats tidy_stats method for class 'summary_emm'
#' @keywords internal
tidy_stats.summary_emm <- function(x, args = NULL) {
  analysis <- list()

  method <- dplyr::case_when(
    "contrast" %in% names(x) ~ "Contrasts",
    stringr::str_detect(attr(x, "estName"), "\\.trend") ~
      "Estimated marginal means of linear trends",
    TRUE ~ "Estimated marginal means"
  )

  analysis$method <- method

  df <- as.data.frame(x)

  # Create a vector of variable names to loop over
  # We can't use the by.vars and pri.vars attributes because they're not always
  # provided (e.g., in the case of contrasts from mvcontrast())
  vars <- names(x)[
    !stringr::str_detect(names(x), paste(
      c(
        "emmean", "estimate", "T.square", "response", "effect.size",
        ".+\\.trend", "prediction",
        "SE",
        "t.ratio",
        "F.ratio",
        "df", "df1", "df2",
        "p.value",
        "lower.CL", "upper.CL",
        "null"
      ),
      collapse = "|"
    ))
  ]

  cl_names <- attr(x, "clNames")
  cl_level <- attr(x, "mesg") |>
    paste(collapse = "") |>
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
            group_df$prediction,
            symbol = "b"
          ) |>
          add_statistic(
            "estimate",
            group_df$T.square,
            symbol = "t²"
          ) |>
          add_statistic(
            "estimate",
            group_df$effect.size,
            symbol = "b",
            interval = "CI",
            level = cl_level,
            lower = group_df[, cl_names[1]],
            upper = group_df[, cl_names[2]]
          )

        if (method == "Estimated marginal means of linear trends") {
          statistics <- add_statistic(
            statistics,
            "estimate",
            group_df[[attr(x, "estName")]],
            symbol = "b",
            interval = "CI",
            level = cl_level,
            lower = group_df[, cl_names[1]],
            upper = group_df[, cl_names[2]]
          )
        }

        statistics <- statistics |>
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
          add_statistic("p", group_df$p.value)

        group_level$statistics <- statistics
      }

      group$groups <- append(group$groups, list(group_level))
    }

    return(group)
  }

  if (length(vars) > 0) {
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
#' @keywords internal
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

    group <- append(group, tidy_stats(x[[name]]))

    # Remove package information because we need to set it only once here and
    # not for every group
    group$package <- NULL

    analysis$groups <- append(analysis$groups, list(group))
  }

  analysis <- add_package_info(analysis, "emmeans")

  return(analysis)
}
