#' @describeIn tidy_stats tidy_stats method for class 'marginaleffects'
#' @export
tidy_stats.marginaleffects <- function(x, args = NULL) {
  analysis <- list(method = "Marginal effects")

  summary <- summary(x)

  groups <- list(name = "Terms")

  for (i in seq_len(nrow(summary))) {
    group <- list(name = summary$term[i])

    group$statistics <- list() |>
      add_statistic(
        "estimate",
        summary$estimate[i],
        symbol = "b",
        interval = "CI",
        level = attr(summary, "conf_level"),
        lower = summary$conf.low[i],
        upper = summary$conf.hig[i]
      ) |>
      add_statistic("SE", summary$std.error[i]) |>
      add_statistic("statistic", summary$statistic[i], "z") |>
      add_statistic("p", summary$p.value[i])

    groups$groups <- append(groups$groups, list(group))
  }

  analysis$groups <- append(analysis$groups, list(groups))

  return(analysis)
}
