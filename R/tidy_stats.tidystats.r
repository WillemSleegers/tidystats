#' @describeIn tidy_stats tidy_stats method for class 'tidystats'
#' @export
tidy_stats.tidystats <- function(x, args = NULL) {
  return(x)
}

#' @describeIn tidy_stats tidy_stats method for class 'tidystats_descriptives'
#' @export
tidy_stats.tidystats_descriptives <- function(x, args = NULL) {
  analysis <- list()

  # Extract variable information
  var_names <- unique(x[["var"]])

  # Set the name if there is only 1 variable
  if (length(var_names) == 1) {
    analysis$name <- var_names
  }

  # Set method
  analysis$method <- "Descriptives"

  # Create a loop function to recursively create groups and extract the
  # statistics
  loop <- function(df, list, group_names, depth) {
    if (length(group_names) == depth) {
      # Create a list to store the statistics in
      statistics <- list()

      # Add statistics
      statistics <- add_statistic(statistics, "missing", df$missing)
      statistics <- add_statistic(statistics, "N", df$N)
      statistics <- add_statistic(statistics, "mean", df$M, "M")
      statistics <- add_statistic(statistics, "standard deviation", df$SD, "SD")
      statistics <- add_statistic(statistics, "standard error", df$SE, "SE")
      statistics <- add_statistic(statistics, "minimum", df$min, "min")
      statistics <- add_statistic(statistics, "maximum", df$maxm, "max")
      statistics <- add_statistic(statistics, "range", df$range)
      statistics <- add_statistic(statistics, "median", df$median, "Mdn")
      statistics <- add_statistic(statistics, "mode", df$mode)
      statistics <- add_statistic(statistics, "skew", df$skew)
      statistics <- add_statistic(statistics, "kurtosis", df$kurtosis)

      # Add statistics to the group
      list$statistics <- statistics
    } else {
      # Increment the depth
      depth <- depth + 1

      # Create a groups list
      groups <- list(name = group_names[depth])

      # Loop over the groups
      for (group_name in unique(df[[groups$name]])) {
        if (!is.na(group_name)) {
          df_group <- df[which(df[[groups$name]] == group_name), ]
        } else {
          df_group <- df[is.na(df[[groups$name]]), ]
        }

        # Create a group list
        # Special case: Convert to character in case the group name is NA
        group <- list(
          name = ifelse(is.na(group_name), "NA", group_name)
        )

        # Loop again
        groups$groups <- append(
          groups$groups,
          list(loop(df_group, group, group_names, depth))
        )
      }

      # Add the groups to the list's groups
      list$groups <- append(list$groups, list(groups))
    }

    return(list)
  }

  # Extract grouping information
  grouping <- group_names(x)

  # Convert the data frame to a base data frame to disable warnings
  df <- as.data.frame(x)

  # Get the groups and statistics and loop over the variables if there are
  # more than one
  if (length(var_names) == 1) {
    analysis <- loop(df, analysis, grouping, 0)
  } else {
    for (var_name in var_names) {
      # Filter the data frame to have only the rows belonging to this variable
      df_var <- df[df$var == var_name, ]

      # Create a list for the variable
      group <- list(name = var_name)

      # Loop
      group <- loop(df_var, group, grouping, 0)

      # Add the lists to the groups element of the analysis
      analysis$groups <- append(analysis$groups, list(group))
    }
  }

  # Add package information
  analysis <- add_package_info(analysis, "tidystats")

  return(analysis)
}

#' @describeIn tidy_stats tidy_stats method for class 'tidystats_counts'
#' @export
tidy_stats.tidystats_counts <- function(x, args = NULL) {
  analysis <- list(method = "Counts")

  loop <- function(df, list, group_names, depth) {
    if (length(group_names) == depth) {
      list$statistics <- list() |>
        add_statistic("n", df$n) |>
        add_statistic("proportion", df$prop, symbol = "p") |>
        add_statistic("pct", df$pct, "%")
    } else {
      depth <- depth + 1

      groups <- list(name = group_names[depth])

      for (group_name in unique(df[[groups$name]])) {
        if (!is.na(group_name)) {
          df_group <- df[which(df[[groups$name]] == group_name), ]
        } else {
          df_group <- df[is.na(df[[groups$name]]), ]
        }

        group <- list()

        # Set the name to the string NA if it is missing
        if (is.na(group_name)) {
          group$name <- "NA"
        } else {
          group$name <- group_name
        }

        groups$groups <- append(groups$groups, list(loop(
          df_group, group,
          group_names, depth
        )))
      }

      list$groups <- append(list$groups, list(groups))
    }

    return(list)
  }

  group_names <- names(x)[!names(x) %in% c("n", "prop", "pct")]

  # Convert the data frame to a base data frame to disable warnings
  df <- as.data.frame(x)

  analysis <- loop(df, analysis, group_names, 0)

  analysis <- add_package_info(analysis, "tidystats")

  return(analysis)
}
