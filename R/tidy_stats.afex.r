#' @describeIn tidy_stats tidy_stats method for class 'afex_aov'
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
        symbol = paste0(intToUtf8(0x03b7), intToUtf8(0x00b2)),
        subscript = "G"
      ) |>
      add_statistic(
        name = "pes",
        value = terms$pes[i],
        symbol = paste0(intToUtf8(0x03b7), intToUtf8(0x00b2)),
        subscript = "p"
      ) |>
      add_statistic(
        name = "p",
        value = terms$`Pr(>F)`[i]
      )

    group$statistics <- statistics

    groups$groups <- append(groups$groups, list(group))
  }
  analysis$groups <- append(analysis$groups, list(groups))

  analysis$anova_type <- attr(x, "type")
  analysis$p_adjustment_method <- attr(x$anova_table, "p_adjust_method")
  analysis$sphericity_correction_method <- attr(x$anova_table, "correction")

  analysis <- add_package_info(analysis, "afex")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'afex_aov'
#' @export
tidy_stats.mixed <- function(x, args = NULL) {
  analysis <- list(method = "Mixed Model ANOVA")

  terms <- x$anova_table

  groups <- list(name = "Terms")

  for (i in seq_len(nrow(terms))) {
    group <- list(name = rownames(terms)[i])

    group$statistics <- list() |>
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
        symbol = paste0(intToUtf8(0x03b7), intToUtf8(0x00b2)),
        subscript = "G"
      ) |>
      add_statistic(
        name = "pes",
        value = terms$pes[i],
        symbol = paste0(intToUtf8(0x03b7), intToUtf8(0x00b2)),
        subscript = "p"
      ) |>
      add_statistic(
        name = "p",
        value = terms$`Pr(>F)`[i]
      )

    groups$groups <- append(groups$groups, list(group))
  }

  analysis$groups <- append(analysis$groups, list(groups))

  analysis$anova_type <- attr(x, "type")

  analysis <- add_package_info(analysis, "afex")

  return(analysis)
}
