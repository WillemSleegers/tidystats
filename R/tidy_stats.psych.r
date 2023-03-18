#' @describeIn tidy_stats tidy_stats method for class 'psych'
#' @export
tidy_stats.psych <- function(x, args = NULL) {
  if ("alpha" %in% class(x)) {
    analysis <- tidy_stats.psych.alpha(x)
  } else if ("corr.test" %in% class(x)) {
    analysis <- tidy_stats.psych.corr.test(x)
  } else if ("mardia" %in% class(x)) {
    analysis <- tidy_stats.psych.mardia(x)
  } else if ("ICC" %in% class(x)) {
    analysis <- tidy_stats.psych.ICC(x)
  }

  analysis <- add_package_info(analysis, "psych")

  return(analysis)
}

tidy_stats.psych.alpha <- function(x) {
  analysis <- list(method = "Reliability analysis")

  analysis$statistics <- list() |>
    add_statistic(
      name = "unstandardized alpha",
      value = x$total$raw_alpha,
      symbol = "α", subscript = "Σ"
    ) |>
    add_statistic(
      name = "standardized alpha",
      value = x$total$std.alpha,
      symbol = "α", subscript = "R"
    ) |>
    add_statistic(
      name = "Guttman's Lambda 6 reliability",
      value = x$total$`G6(smc)`,
      symbol = "Guttman's λ", subscript = "6"
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

  # Create a group for the 95% confidence boundaries, if there are any
  if (!is.null(x$feldt) || !is.null(x$total$ase) || !is.null(x$boot.ci)) {
    group <- list(name = "95% confidence boundaries")

    if (!is.null(x$feldt)) {
      group_feldt <- list(name = "Feldt")
      statistics <- list()

      statistics <- add_statistic(statistics, "alpha", x$total$raw_alpha,
        symbol = "α", interval = "CI", level = .95,
        lower = x$feldt$lower.ci[[1]], upper = x$feldt$upper.ci[[1]]
      )

      group_feldt$statistics <- statistics
      group$groups <- append(group$groups, list(group_feldt))
    }

    if (!is.null(x$total$ase)) {
      group_duhachek <- list(name = "Duhachek")
      statistics <- list()

      statistics <- add_statistic(statistics, "alpha", x$total$raw_alpha,
        symbol = "α", interval = "CI", level = .95,
        lower = x$total$raw_alpha - 1.96 * x$total$ase,
        upper = x$total$raw_alpha + 1.96 * x$total$ase
      )

      group_duhachek$statistics <- statistics
      group$groups <- append(group$groups, list(group_duhachek))
    }

    if (!is.null(x$boot.ci)) {
      group_bootstrapped <- list(name = "bootstrapped")
      statistics <- list()

      statistics <- add_statistic(statistics, "alpha", x$boot.ci[2],
        symbol = "α", interval = "CI", level = .95,
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
      symbol = "α", subscript = "Σ"
    )
    statistics <- add_statistic(statistics, "standardized alpha",
      x$total$std.alpha[i],
      symbol = "α", subscript = "R"
    )
    statistics <- add_statistic(statistics, "Guttman's Lambda 6 reliability",
      x$total$`G6(smc)`,
      symbol = "Guttman's λ", subscript = "6"
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

tidy_stats.psych.corr.test <- function(x) {
  analysis <- list(method = "Correlation") # TODO detect kendall or spearman

  if (x$adjust == "none") {
    warning("Only saving unadjusted statistics.")
  } else {
    warning("Only saving adjusted statistics.")
  }

  # Check if there is only 1 pair, or multiple
  if (length(rownames(x$r)) == 1) {
    # Create a list for the statistics of this single pair
    # statistics <- list()

    # TODO: figure out number of pairs
  }

  analysis$adjust <- x$adjust

  output$method <- "Correlations"

  # Create an empty pairs list
  pairs <- list()

  # Extract variable names
  rownames <- rownames(x$r)
  colnames <- colnames(x$r)

  # Determine whether the correlation matrix is symmetric or asymmetric
  if (identical(rownames, colnames)) {
    I <- choose(length(rownames), 2)
    symmetric <- TRUE
  } else {
    I <- length(rownames) * length(colnames)
    symmetric <- FALSE
  }

  # Tidy statistics
  rs <- tidy_matrix(x$r, symmetric = symmetric)
  SEs <- tidy_matrix(x$se, symmetric = symmetric)
  ts <- tidy_matrix(x$t, symmetric = symmetric)
  ps <- tidy_matrix(t(x$p), symmetric = symmetric)
  ps_adjusted <- tidy_matrix(x$p, symmetric = symmetric)

  # Check if there are confidence intervals
  if (!is.null(x$ci)) {
    # Figure out the level
    alpha <- as.character(x$Call)[which(names(x$Call) == "alpha")]

    if (length(alpha) == 0) {
      level <- .95
    } else {
      level <- 1 - as.numeric(alpha)
    }
  }

  if (length(x$n) == 1) {
    n <- x$n
  } else {
    ns <- tidy_matrix(x$n, symmetric = symmetric)
  }

  # Loop over the pairs
  for (i in 1:I) {
    pair <- list()

    # Set names
    names <- list()
    names[[1]] <- rs$name1[i]
    names[[2]] <- rs$name2[i]
    pair$names <- names

    # Set statistics
    if (length(x$n) == 1) {
      pair$statistics$n <- n
    } else {
      pair$statistics$n <- ns$value[i]
    }
    pair$statistics$estimate$name <- "r"
    pair$statistics$estimate$value <- rs$value[i]
    pair$statistics$SE <- SEs$value[i]
    pair$statistics$statistic$name <- "t"
    pair$statistics$statistic$value <- ts$value[i]
    pair$statistics$p <- ps$value[i]

    if (x$adjust != "none") {
      pair$statistics$p_adjusted <- ps_adjusted$value[i]
    }

    if (!is.null(x$ci)) {
      pair$statistics$CI$CI_level <- level
      pair$statistics$CI$CI_lower <- x$ci$lower[i]
      pair$statistics$CI$CI_upper <- x$ci$upper[i]
    }

    if (!is.null(x$ci)) {
      pair$statistics$CI_adjusted$CI_level <- level
      pair$statistics$CI_adjusted$CI_lower <- x$ci.adj$lower[i]
      pair$statistics$CI_adjusted$CI_upper <- x$ci.adj$upper[i]
    }

    pairs[[i]] <- pair
  }

  # Add pairs to output
  output$pairs <- pairs

  # Set multiple test adjustment method
  output$multiple_test_adjustment <- x$adjust
}

tidy_stats.psych.mardia <- function(x) {
  analysis <- list(method = "Mardia's test")

  statistics <- list()

  statistics <- add_statistic(statistics, "number of observations", x$n.obs,
    symbol = "N"
  )
  statistics <- add_statistic(statistics, "number of variables", x$n.var,
    symbol = "k"
  )

  analysis$statistics <- statistics

  # Create a group for the skew statistics
  group <- list(name = "skew")

  statistics <- list()

  statistics <- add_statistic(statistics, "estimate", x$b1p,
    symbol = "b",
    subscript = "1, p"
  )
  statistics <- add_statistic(statistics, "skew", x$skew)
  statistics <- add_statistic(statistics, "p", x$p.skew)

  group$statistics <- statistics
  analysis$groups <- append(analysis$groups, list(group))

  # Create a group for the small sample skew statistics
  group <- list(name = "small sample skew")

  statistics <- list()

  statistics <- add_statistic(statistics, "estimate", x$b1p,
    symbol = "b",
    subscript = "1, p"
  )
  statistics <- add_statistic(statistics, "skew", x$small.skew)
  statistics <- add_statistic(statistics, "p", x$p.small)

  group$statistics <- statistics
  analysis$groups <- append(analysis$groups, list(group))

  # Create a group for the kurtosis statistics
  group <- list(name = "kurtosis")

  statistics <- list()

  statistics <- add_statistic(statistics, "estimate", x$b2p,
    symbol = "b",
    subscript = "2, p"
  )
  statistics <- add_statistic(statistics, "kurtosis", x$kurtosis)
  statistics <- add_statistic(statistics, "p", x$p.kurt)

  group$statistics <- statistics
  analysis$groups <- append(analysis$groups, list(group))
}

tidy_stats.psych.ICC <- function(x) {
  analysis <- list(method = "Intraclass Correlations")

  results <- x$results

  analysis$statistics <- list() |>
    add_statistic(
      name = "Number of subjects",
      value = x$n.obs,
    ) |>
    add_statistic(
      name = "Number of judges",
      value = x$n.judge
    )

  for (i in seq_len(nrow(results))) {
    group <- list(
      name = rownames(results)[i],
      type = results$type[i]
    )

    group$statistics <- list() |>
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

    analysis$groups <- append(analysis$groups, list(group))
  }

  return(analysis)
}
