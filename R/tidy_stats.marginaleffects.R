#' @describeIn tidy_stats tidy_stats method for class 'predictions'
tidy_stats.predictions <- function(x, args = NULL) {
  analysis <- list(method = "Average (marginal) estimates")

  # Figure out the grouping variables; we can't use attr(x, "by") because it
  # can return more groups than there are rows in the output (see the 2nd
  # example with multinom())
  vars <- names(x)[
    !grepl(
      paste(
        c(
          "rowid", "term", "estimate", "std.error", "statistic", "p.value",
          "conf.low", "conf.high"
        ),
        collapse = "|"
      ),
      names(x)
    )
  ]

  if (length(vars) > 0) {
    analysis <- add_group(analysis, vars, x)
  } else {
    analysis <- add_statistics(analysis, x)
  }

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'comparisons'
tidy_stats.comparisons <- function(x, args = NULL) {
  analysis <- list(method = "Average (marginal) estimates")

  x <- as.data.frame(x)

  names(x)[which(names(x) == "contrast")] <- unique(x$term)
  names(x) <- gsub("contrast_", "", names(x))

  vars <- names(x)[
    !grepl(
      paste(
        c(
          "rowid", "term", "estimate", "std.error", "statistic", "p.value",
          "conf.low", "conf.high"
        ),
        collapse = "|"
      ),
      names(x)
    )
  ]

  if (nrow(x) > 1) {
    analysis <- add_group(analysis, vars, x)
  } else {
    analysis <- add_statistics(analysis, x)
  }

  return(analysis)
}

add_statistics <- function(list, x) {
  list$statistics <- list() |>
    add_statistic(
      "estimate",
      x$estimate,
      symbol = "b",
      interval = "CI",
      level = attr(x, "conf_level"),
      lower = x$conf.low,
      upper = x$conf.hig
    ) |>
    add_statistic("SE", x$std.error) |>
    add_statistic("statistic", x$statistic, "z") |>
    add_statistic("p", x$p.value)

  return(list)
}

add_group <- function(list, vars, x) {
  var <- vars[1]
  group <- list(name = var)
  levels <- unique(x[, var])

  for (level in levels) {
    group_level <- list(name = as.character(level))
    x_level <- x[x[, var] == level, ]

    if (length(vars) > 1) {
      group_level <- add_group(group_level, vars[-1], x_level)
    } else {
      group_level <- add_statistics(group_level, x_level)
    }

    group$groups <- append(group$groups, list(group_level))
  }

  list$groups <- append(list$groups, list(group))

  return(list)
}
