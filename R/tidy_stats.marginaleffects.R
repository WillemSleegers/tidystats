#' @describeIn tidy_stats tidy_stats method for class 'predictions'
tidy_stats.predictions <- function(x, args = NULL) {
  analysis <- list(method = "Average (marginal) estimates")

  # Check whether the output is the result of functions like avg_predictions()
  # by comparing the rows in the data frame to the rows of the model
  if (nrow(x) == length(attr(x, "model")$fitted.values)) {
    stop(
      paste(
        "Unsupported data. Support is limited to results from functions that",
        "return statistics for groups (e.g., avg_predictions()) and not for",
        "each row in the data."
      )
    )
  }

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
    analysis <- add_marginaleffects_group(analysis, vars, x)
  } else {
    analysis <- add_marginaleffects_statistics(analysis, x)
  }

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'comparisons'
tidy_stats.comparisons <- function(x, args = NULL) {
  analysis <- list(method = "Average (marginal) estimates")

  terms <- unique(x$term)

  analysis <- add_marginaleffects_terms(analysis, terms, x)

  return(analysis)
}

add_marginaleffects_terms <- function(list, terms, x) {
  group_terms <- list(name = "Terms")

  # Two special cases:
  # Cross contrasts
  # Custom contrasts
  if (all(terms == "cross")) {
    # Combine the multiple terms (if any) into a single name
    group_term <- list(
      name = paste(names(attr(x, "variables")), collapse = ",")
    )

    # Combine the multiple contrast columns into a single column
    x <- tidyr::unite(
      x,
      col = "contrast", dplyr::starts_with("contrast"), sep = ","
    )

    group_term <- add_marginaleffects_contrasts(
      group_term,
      unique(x$contrast),
      x
    )

    group_terms$groups <- append(group_terms$groups, list(group_term))
  } else if (all(terms == "custom")) {
    group_term <- list(name = "custom")

    # Name the contrasts by their row id and add the column to the data frame
    contrasts <- seq_len(nrow(x))
    x$contrast <- contrasts

    group_term <- add_marginaleffects_contrasts(
      group_term,
      contrasts,
      x
    )

    group_terms$groups <- append(group_terms$groups, list(group_term))
  } else {
    for (term in terms) {
      group_term <- list(name = term)

      x_term <- x[x[, "term"] == term, ]

      contrasts <- unique(x_term$contrast)

      if (!is.null(contrasts)) {
        group_term <- add_marginaleffects_contrasts(
          group_term,
          unique(x_term$contrast),
          x_term
        )
      } else {
        group_term <- add_marginaleffects_statistics(group_term, x_term)
      }

      group_terms$groups <- append(group_terms$groups, list(group_term))
    }
  }

  list$groups <- group_terms

  return(list)
}

add_marginaleffects_contrasts <- function(list, contrasts, x) {
  group_contrasts <- list(name = "Contrasts")

  for (contrast in contrasts) {
    group_contrast <- list(name = contrast)

    by <- attr(x, "by")

    if (!is.null(by)) {
      group_by <- list(name = "By")

      group_by <- add_marginaleffects_group(
        group_by, by, x[x[, "contrast"] == contrast, ]
      )

      group_contrast$groups <- append(
        group_contrast$groups,
        list(group_by)
      )

      group_contrasts$groups <- append(
        group_contrasts$groups,
        list(group_contrast)
      )
    } else {
      group_contrast <- add_marginaleffects_statistics(
        group_contrast,
        x[x[, "contrast"] == contrast, ]
      )

      group_contrasts$groups <- append(
        group_contrasts$groups,
        list(group_contrast)
      )
    }
  }

  list$groups <- group_contrasts

  return(list)
}

add_marginaleffects_group <- function(list, vars, x) {
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

add_marginaleffects_statistics <- function(list, x) {
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
