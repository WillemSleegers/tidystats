#' @describeIn tidy_stats tidy_stats method for class 'emmGrid'
#' @export
tidy_stats.emmGrid <- function(x, args = NULL) {
  return(tidy_stats(summary(x)))
}

#' @describeIn tidy_stats tidy_stats method for class 'summary_emm'
#' @export
tidy_stats.summary_emm <- function(x, args = NULL) {
  analysis <- list()

  df <- as.data.frame(x)

  by_vars <- attr(x, "by.vars")
  pri_vars <- attr(x, "pri.vars")
  vars <- c(by_vars, pri_vars)

  df <- dplyr::mutate(df, dplyr::across(dplyr::any_of(vars), as.factor))

  group_statistics <- function(vars, df) {
    var <- vars[1]
    group <- list(name = var)

    levels <- levels(df[, var])
    cl_names <- attr(x, "clNames")
    cl_level <- str_extract(attr(x, "mesg"), "Confidence level used: ", )


    for (level in levels) {
      group_level <- list(name = level)
      group_df <- df[df[, var] == level, ]

      if (length(vars) > 1) {
        group_level$groups <- append(
          group_level$groups,
          list(group_statistics(vars[-1], group_df))
        )
      } else {
        statistics <- list() %>%
          add_statistic(
            "EMM",
            group_df$emmean,
            interval = "CI",
            lower = group_df[, clNames[1]],
            upper = group_df[, clNames[2]]
          ) %>%
          add_statistic("SE", group_df$SE) %>%
          add_statistic("df", group_df$df)
        group_level$statistics <- statistics
      }

      group$groups <- append(group$groups, list(group_level))
    }

    return(group)
  }

  analysis$groups <- append(analysis$groups, list(group_statistics(vars, df)))

  analysis$type <- attr(x, "type")

  analysis <- add_package_info(analysis, "emmeans")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'emm_list'
#' @export
tidy_stats.emm_list <- function(x, args = NULL) {
  analysis <- list()

  for (i in i:length(x)) {
    group <- tidy_stats.emmGrid(x[[i]])

    # Remove package information because we'll set it at the analysis level
    group$package <- NULL

    analysis$groups <- append(analysis$groups, list(group))
  }

  analysis <- add_package_info(analysis, "emmeans")

  return(analysis)
}
