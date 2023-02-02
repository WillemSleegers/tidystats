#' @describeIn tidy_stats tidy_stats method for class 'afex_aov'
#' @export
tidy_stats.afex_aov <- function(x, args = NULL) {
  analysis <- list(method = "ANOVA")

  terms <- x$anova_table

  groups <- list(name = "Terms")

  for (i in seq_len(nrow(terms))) {
    group <- list(name = rownames(terms)[i])

    statistics <- list() |>
      add_statistic(
        name = "df numerator",
        value = terms$`num Df`[i],
        symbol = "df",
        subscript = "num."
      ) |>
      add_statistic(
        name = "df denominator",
        value = terms$`den Df`[i],
        symbol = "df",
        subscript = "den."
      ) |>
      add_statistic(
        name = "MSE",
        value = terms$MSE[i]
      ) |>
      add_statistic(
        name = "statistic",
        value = terms$`F`[i],
        symbol = "F"
      ) |>
      add_statistic(
        name = "ges",
        value = terms$ges[i],
        symbol = "η²",
        subscript = "G"
      ) |>
      add_statistic(
        name = "pes",
        value = terms$pes[i],
        symbol = "η²",
        subscript = "p"
      ) |>
      add_statistic(
        name = "p",
        value = terms$`Pr(>F)`[i]
      )

    # Add statistics to the group
    group$statistics <- statistics

    # Add the group to the groups list
    groups$groups <- append(groups$groups, list(group))
  }

  # Add the groups to the groups list on the analysis list
  analysis$groups <- append(analysis$groups, list(groups))

  # Add additional information
  analysis$anova_type <- attr(x, "type")
  analysis$p_adjustment_method <- attr(x$anova_table, "p_adjust_method")
  analysis$sphericity_correction_method <- attr(x$anova_table, "correction")

  # Add package information
  analysis <- add_package_info(analysis, "afex")

  return(analysis)
}
