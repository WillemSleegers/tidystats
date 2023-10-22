#' @describeIn tidy_stats tidy_stats method for class 'psych'
#' @export
tidy_stats.psych <- function(x, args = NULL) {
  if ("alpha" %in% class(x)) {
    analysis <- tidy_stats_psych_alpha(x)
  } else if ("corr.test" %in% class(x)) {
    analysis <- tidy_stats_psych_corr_test(x)
  } else if ("mardia" %in% class(x)) {
    analysis <- tidy_stats_psych_mardia(x)
  } else if ("ICC" %in% class(x)) {
    analysis <- tidy_stats_psych_ICC(x)
  }

  analysis <- add_package_info(analysis, "psych")

  return(analysis)
}

tidy_stats_psych_alpha <- function(x) {
  analysis <- list(method = "Reliability analysis")

  group <- list(name = "Reliability analysis")

  group$statistics <- list() |>
    add_statistic(
      name = "unstandardized alpha",
      value = x$total$raw_alpha,
      symbol = symbol("alpha"), subscript = intToUtf8(0x03b1)
    ) |>
    add_statistic(
      name = "standardized alpha",
      value = x$total$std.alpha,
      symbol = symbol("alpha"), subscript = "R"
    ) |>
    add_statistic(
      name = "Guttman's Lambda 6 reliability",
      value = x$total$`G6(smc)`,
      symbol = symbol("guttmans_lambda"), subscript = "6"
    ) |>
    add_statistic(
      name = "mean interitem correlation",
      value = x$total$average_r,
      symbol = "IIC", subscript = "M"
    ) |>
    add_statistic(
      name = "signal-to-noise ratio",
      value = x$total$`S/N`,
      symbol = "S/N"
    ) |>
    add_statistic(
      name = "standard error",
      value = x$total$ase,
      symbol = "SE"
    ) |>
    add_statistic(
      name = "mean",
      value = x$total$mean,
      symbol = "M"
    ) |>
    add_statistic(
      name = "standard deviation",
      value = x$total$sd,
      symbol = "SD"
    ) |>
    add_statistic(
      name = "median interitem correlation",
      value = x$total$median_r,
      symbol = "IIC", subscript = "Mdn"
    )

  analysis$groups <- append(analysis$groups, list(group))

  # Create a group for the 95% confidence boundaries, if there are any
  if (!is.null(x$feldt) || !is.null(x$total$ase) || !is.null(x$boot.ci)) {
    group <- list(name = "95% confidence boundaries")

    if (!is.null(x$feldt)) {
      group_feldt <- list(name = "Feldt")
      statistics <- list()

      statistics <- add_statistic(statistics, "alpha", x$total$raw_alpha,
        symbol = symbol("alpha"), interval = "CI", level = .95,
        lower = x$feldt$lower.ci[[1]], upper = x$feldt$upper.ci[[1]]
      )

      group_feldt$statistics <- statistics
      group$groups <- append(group$groups, list(group_feldt))
    }

    if (!is.null(x$total$ase)) {
      group_duhachek <- list(name = "Duhachek")
      statistics <- list()

      statistics <- add_statistic(statistics, "alpha", x$total$raw_alpha,
        symbol = symbol("alpha"), interval = "CI", level = .95,
        lower = x$total$raw_alpha - 1.96 * x$total$ase,
        upper = x$total$raw_alpha + 1.96 * x$total$ase
      )

      group_duhachek$statistics <- statistics
      group$groups <- append(group$groups, list(group_duhachek))
    }

    if (!is.null(x$boot.ci)) {
      group_bootstrapped <- list(name = "bootstrapped")
      statistics <- list()

      statistics <- add_statistic(
        statistics, "alpha", x$boot.ci[2],
        symbol = symbol("alpha"), interval = "CI", level = .95,
        lower = x$boot.ci[1], upper = x$boot.ci[3]
      )

      group_bootstrapped$statistics <- statistics
      group$groups <- append(group$groups, list(group_bootstrapped))
    }

    analysis$groups <- append(analysis$groups, list(group))
  }

  # Create a group for the the reliability if an item is dropped statistics
  group <- list(name = "Reliability if an item is dropped")

  # Loop over the items
  for (i in seq_len(nrow(x$alpha.drop))) {
    # Create a list for the item
    item <- list(name = rownames(x$alpha.drop)[i])

    # Create a statistics list and add the item statistics
    statistics <- list()

    statistics <- add_statistic(statistics, "unstandardized alpha",
      x$total$raw_alpha[i],
      symbol = symbol("alpha"), subscript = symbol("sigma")
    )
    statistics <- add_statistic(statistics, "standardized alpha",
      x$total$std.alpha[i],
      symbol = symbol("alpha"), subscript = "R"
    )
    statistics <- add_statistic(statistics, "Guttman's Lambda 6 reliability",
      x$total$`G6(smc)`,
      symbol = symbol("guttmans_lambda"), subscript = "6"
    )
    statistics <- add_statistic(statistics, "mean interitem correlation",
      x$total$average_r[i],
      symbol = "IIC", subscript = "M"
    )
    statistics <- add_statistic(statistics, "signal-to-noise ratio",
      x$total$`S/N`[i],
      symbol = "S/N"
    )
    statistics <- add_statistic(statistics, "standard error", x$total$ase[i],
      symbol = "SE"
    )
    statistics <- add_statistic(statistics, "mean", x$total$mean[i],
      symbol = "M"
    )
    statistics <- add_statistic(statistics, "standard deviation",
      x$total$sd[i],
      symbol = "SD"
    )
    statistics <- add_statistic(statistics, "variance interitem correlation",
      x$total$var.r[i],
      symbol = "IIC", subscript = "var"
    )
    statistics <- add_statistic(statistics, "median interitem correlation",
      x$total$med.r[i],
      symbol = "IIC", subscript = "Mdn"
    )

    # Add statistics to the group
    item$statistics <- statistics

    # Add item to the group
    group$groups <- append(group$groups, list(item))
  }

  # Add the reliability if an item is dropped group to the analysis groups
  analysis$groups <- append(analysis$groups, list(group))

  # Create a group for the item statistics
  group <- list(name = "Item statistics")

  # Loop over the items
  for (i in seq_len(nrow(x$item.stats))) {
    # Create a list for the item
    item <- list(name = rownames(x$item.stats)[i])

    # Create a statistics list and add the item statistics
    statistics <- list()

    statistics <- add_statistic(statistics, "number of complete cases",
      x$item.stats$n[i],
      symbol = "n"
    )
    statistics <- add_statistic(statistics,
      "total score correlation with standardized items",
      x$item.stats$r[i],
      symbol = "r", subscript = "std"
    )
    statistics <- add_statistic(statistics, "total score correlation",
      x$item.stats$raw.r[i],
      symbol = "r", subscript = "raw"
    )
    statistics <- add_statistic(statistics,
      "total score correlation with standardized items",
      x$item.stats$std.r[i],
      symbol = "r", subscript = "std"
    )
    statistics <- add_statistic(statistics,
      "corrected item whole correlation",
      x$item.stats$r.cor[i],
      symbol = "r", subscript = "cor"
    )
    statistics <- add_statistic(statistics,
      "item whole correlation without this item",
      x$item.stats$r.drop[i],
      symbol = "r", subscript = "drop"
    )
    statistics <- add_statistic(statistics, "mean", x$item.stats$mean[i],
      symbol = "M"
    )
    statistics <- add_statistic(statistics, "standard deviation",
      x$item.stats$sd[i],
      symbol = "SD"
    )

    item$statistics <- statistics

    group$groups <- append(group$groups, list(item))
  }

  analysis$groups <- append(analysis$groups, list(group))

  return(analysis)
}

tidy_stats_psych_corr_test <- function(x) {
  # TODO detect kendall or spearman
  analysis <- list(method = "Correlation")

  if (x$sym) {
    pairs <- t(combn(rownames(x$r), 2))
  } else {
    pairs <- expand.grid(rownames(x$r), colnames(x$r))
  }

  alpha <- as.character(x$Call)[which(names(x$Call) == "alpha")]
  if (length(alpha) == 0) {
    level <- .95
  } else {
    level <- 1 - as.numeric(alpha)
  }

  # Unadjusted statistics
  group_pairs <- list(name = "Pairs (unadjusted)")

  for (i in seq_len(nrow(pairs))) {
    group <- list(names = list(
      list(name = pairs[i, 1]),
      list(name = pairs[i, 2])
    ))

    if (length(x$n) == 1) {
      n <- x$n
    } else {
      n <- x$n[pairs[i, 1], pairs[i, 2]]
    }

    statistics <- list() |>
      add_statistic("n", n) |>
      add_statistic(
        "estimate",
        x$r[pairs[i, 1], pairs[i, 2]],
        symbol = "r",
        interval = "CI",
        level = level,
        lower = x$ci2$lower[i],
        upper = x$ci2$upper[i]
      ) |>
      add_statistic("standard error", x$se[pairs[i, 1], pairs[i, 2]], "SE") |>
      add_statistic("statistic", x$t[pairs[i, 1], pairs[i, 2]], "t") |>
      add_statistic("df", n - 2) |>
      add_statistic("p", x$p[pairs[i, 1], pairs[i, 2]])

    group$statistics <- statistics
    group_pairs$groups <- append(group_pairs$groups, list(group))
  }

  # Adjusted statistics
  if (x$adjust != "none") {
    group_pairs_adjusted <- list(name = "Pairs (adjusted)")

    for (i in seq_len(nrow(pairs))) {
      group <- list(names = list(
        list(name = pairs[i, 1]),
        list(name = pairs[i, 2])
      ))

      if (length(x$n) == 1) {
        n <- x$n
      } else {
        n <- x$n[pairs[i, 1], pairs[i, 2]]
      }

      statistics <- list() |>
        add_statistic("n", n) |>
        add_statistic(
          "estimate",
          x$r[pairs[i, 1], pairs[i, 2]],
          symbol = "r",
          interval = "CI",
          level = level,
          lower = x$ci.adj$lower[i],
          upper = x$ci.adj$upper[i]
        ) |>
        add_statistic("standard error", x$se[pairs[i, 1], pairs[i, 2]], "SE") |>
        add_statistic("statistic", x$t[pairs[i, 1], pairs[i, 2]], "t") |>
        add_statistic("df", n - 2) |>
        add_statistic("p", x$ci2$p.adj[i]) |>
        add_statistic("p", x$ci2$pa[i])

      group$statistics <- statistics
      group_pairs_adjusted$groups <- append(
        group_pairs_adjusted$groups,
        list(group)
      )
    }
  }

  if (x$adjust == "none") {
    analysis$groups <- append(analysis$groups, list(group_pairs))
  } else {
    analysis$groups <- append(analysis$groups, list(group_pairs))
    analysis$groups <- append(analysis$groups, list(group_pairs_adjusted))
  }

  analysis$adjust <- x$adjust

  return(analysis)
}

tidy_stats_psych_mardia <- function(x) {
  analysis <- list(method = "Mardia's test")

  # Create a group for the skew statistics
  group <- list(name = "Skew")

  group$statistics <- list() |>
    add_statistic("number of observations", x$n.obs, symbol = "n") |>
    add_statistic("number of variables", x$n.var, symbol = "k") |>
    add_statistic("estimate", x$b1p, symbol = "b", subscript = "1, p") |>
    add_statistic("skew", x$skew) |>
    add_statistic("p", x$p.skew)

  analysis$groups <- append(analysis$groups, list(group))

  # Create a group for the small sample skew statistics
  group <- list(name = "Small sample skew")

  group$statistics <- list() |>
    add_statistic("number of observations", x$n.obs, symbol = "n") |>
    add_statistic("number of variables", x$n.var, symbol = "k") |>
    add_statistic("estimate", x$b1p, symbol = "b", subscript = "1, p") |>
    add_statistic("skew", x$small.skew) |>
    add_statistic("p", x$p.small)

  analysis$groups <- append(analysis$groups, list(group))

  # Create a group for the kurtosis statistics
  group <- list(name = "kurtosis")

  group$statistics <- list() |>
    add_statistic("estimate", x$b2p, symbol = "b", subscript = "2, p") |>
    add_statistic("kurtosis", x$kurtosis) |>
    add_statistic("p", x$p.kurt)

  analysis$groups <- append(analysis$groups, list(group))

  return(analysis)
}

tidy_stats_psych_ICC <- function(x) {
  analysis <- list(method = "Intraclass Correlations")

  results <- x$results

  group_coefficients <- list(name = "Intraclass correlation coefficients")

  for (i in seq_len(nrow(results))) {
    group <- list(
      name = rownames(results)[i],
      type = results$type[i]
    )

    group$statistics <- list() |>
      add_statistic(
        name = "Number of subjects",
        value = x$n.obs,
        symbol = "n"
      ) |>
      add_statistic(
        name = "Number of judges",
        value = x$n.judge,
        symbol = "k"
      ) |>
      add_statistic(
        name = "ICC",
        value = results$ICC[i],
        interval = "CI",
        level = 1 - x$Call$alpha,
        lower = results$`lower bound`[i],
        upper = results$`upper bound`[i]
      ) |>
      add_statistic("statistic", results$F[i], symbol = "F") |>
      add_statistic(
        "df numerator", results$df1[i],
        symbol = "df", subscript = "num."
      ) |>
      add_statistic(
        "df denominator", results$df2[i],
        symbol = "df", subscript = "den."
      ) |>
      add_statistic("p", results$p[i])

    group_coefficients$groups <- append(group_coefficients$groups, list(group))
  }

  analysis$groups <- append(analysis$groups, list(group_coefficients))

  return(analysis)
}
