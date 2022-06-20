#' Helper functions in tidystats
#'
#' @description Functions used under the hood in the \code{tidystats} package.
#'
#' @describeIn tidy_matrix
#' Function to convert matrix objects to a tidy data frame.
#'
#' @param m A matrix.

tidy_matrix <- function(m, symmetric = TRUE) {
  # Check whether there are row and column names
  if (!length(rownames(m)) > 0) {
    stop("Matrix has no row names.")
  }

  if (!length(colnames(m)) > 0) {
    stop("Matrix has no column names.")
  }

  # Check if the matrix is indeed symmetric
  if (symmetric) {
    if (sum(rownames(m) == colnames(m)) != length(rownames(m))) {
      stop("Matrix row and column names do not match.")
    }
  }

  # Remove the diagonal and duplicate values in case of a symmetric matrix
  if (symmetric) {
    m[lower.tri(m, diag = TRUE)] <- NA
  }

  # Tidy the matrix into a data frame
  df <- m %>%
    as.matrix() %>%
    tibble::as_tibble(rownames = "name1") %>%
    tidyr::pivot_longer(-name1, names_to = "name2", values_to = "value") %>%
    dplyr::filter(!is.na(value))

  return(df)
}

add_statistic <- function(list, name, value, symbol = NULL, subscript = NULL,
  interval = NULL, level = NULL, lower = NULL, upper = NULL) {

  if (!is.null(value)) {
    if (!anyNA(value) || all(name == "p")) {
      new_list <- list()
      new_list$name <- name

      if (!is.null(symbol)) {
        if (!is.na(symbol)) new_list$symbol <- symbol
      }

      if (!is.null(subscript)) {
        if (!is.na(subscript)) new_list$subscript <- subscript
      }

      new_list$value <- value

      if (!is.null(level)) {
        if (!is.na(level)) {
          new_list$interval <- interval
          new_list$level <- level
          new_list$lower <- lower
          new_list$upper <- upper
        }
      }

      list <- append(list, list(new_list))
    }
  }

  return(list)
}

add_package_info <- function(list, package) {
  list$package <- list(
    name = package,
    version = getNamespaceVersion(package)[[1]]
  )

  return(list)
}


df_to_group <- function(table_name, x, symbols = NULL, 
                 subscripts = NULL, na_rm = FALSE, ci_title = "CI", 
                 ci_est_name = NULL, ci_lim_names = c("CIlower", "CIupper"),
                 ci_level = 0.95) {
  if (is.null(symbols)) {
    symbols = c(
      "tau" = "τ",
      "^2" = "²",
      "sigma" = "σ",
      "rho" = "ρ",
      "pval" = "p",
      "p.value" = "p",
      "p value" = "p",
      "zval" = "z",
      "z value" = "z",
      "tval" = "t",
      "p value" = "p",
      "std.error" = "SE",
      "conf.low" = "CIlower",
      "conf.high" = "CIupper",
      "ci.lb" = "CIlower",
      "ci.ub" = "CIupper",
      "lower.ci" = "CIlower",
      "upper.ci" = "CIupper"
    )
  }
  out_df <- as.data.frame(x)
  indices <- sapply(out_df, is.factor)
  out_df[indices] <- lapply(out_df[indices], as.character)
  if (na_rm) {
    out_df = Filter(function(x)!all(is.na(x) | x == ""), out_df)
  }
  if (ncol(out_df) == 0 || nrow(out_df) < 1 ) {
     return(NULL)
  }
  if (is.vector(symbols) && !is.null(names(symbols)) &&
      !any(is.na(names(symbols)))) {
    for (replacer in names(symbols)) {
      colnames(out_df) = gsub(replacer,
        symbols[replacer], colnames(out_df), fixed = TRUE)
      rownames(out_df) = gsub(replacer,
        symbols[replacer], rownames(out_df), fixed = TRUE)
    }
  }
  if (!is.character(subscripts)) {
    subscript_found = NULL
  } else {
    subscript_found = TRUE
  }
  if (any(rownames(out_df) == "")) {
    rownames(out_df)[rownames(out_df) == ""] = 1:nrow(out_df)[rownames(out_df) == ""]
  }

  ci_names = c(ci_est_name, ci_lim_names)
  if (!(
    is.character(ci_est_name) &&
    is.character(ci_lim_names) && length(ci_lim_names) == 2 &&
    all(ci_names %in% colnames(out_df))
  )) {
    ci_est_name = FALSE
  }
  
  groups <- list(name = paste("Table:", table_name))
  # Loop over the coefficients and add statistics to a group list
  for (i in 1:nrow(out_df)) {
    # Create a new group list
    group <- list()
    # Add the name and type of the coefficient
    group$name <- rownames(out_df)[i]
    # Create a new statistics list
    statistics <- list()
    for (j in 1:ncol(out_df)) {
      if (!is.null(subscript_found)) {
        subscript_found = endsWith(colnames(out_df)[j], subscripts)
      }
      if (any(subscript_found)) {
        subscript = subscripts[subscript_found][1]
        col_name = gsub(paste0(subscript, "$"), "", colnames(out_df)[j])
        cell_val = ifelse(is.na(out_df[i, j]), "-", out_df[i, j])
      } else {
        col_name = colnames(out_df)[j]
        cell_val = ifelse(is.na(out_df[i, j]), "-", out_df[i, j])
        subscript = NULL
      }
      if (isFALSE(ci_est_name) || (!colnames(out_df)[j] %in% ci_names)) {
        statistics <-
          add_statistic(statistics, col_name, cell_val, subscript = subscript)
      } else if (ci_est_name == colnames(out_df)[j]) {
        statistics <-
          add_statistic(statistics, col_name, cell_val, subscript = subscript, 
            interval = ci_title, level = ci_level, lower = out_df[[ci_lim_names[1]]][i], 
            upper = out_df[[ci_lim_names[2]]][i])
      }
    }

    # Add statistics to the group
    group$statistics <- statistics
    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }

  return(list(groups))
}

replacers = list("tau" = "τ",
                 "^2" = "²",
                 "sigma" = "σ",
                 "rho" = "ρ",
                 "pval" = "p",
                 "zval" = "z",
                 "tval" = "ρ")

ci_df_to_group <- function(name, df, level) {
  df = as.data.frame(df)
  if (any(rownames(df) == "")) {
    rownames(df)[rownames(df) == ""] = 1:nrow(df)[rownames(df) == ""]
  }
  for (replacer in names(replacers)) {
    rownames(df) = gsub(replacer, replacers[[replacer]], rownames(df), fixed = TRUE)
  }
  groups <- list(name = paste("Table:", name))
  # Loop over the coefficients and add statistics to a group list
  for (i in 1:nrow(df)) {
    # Create a new group list
    group <- list()
    # Add the name and type of the coefficient
    group$name <- rownames(df)[i]
    # Create a new statistics list
    statistics <- list()
    if (!is.null(level)) {
      statistics <-
        add_statistic(statistics, "estimate", df$estimate[i], interval = "CI", 
          level = level, lower = df$ci.lb[i], upper = df$ci.ub[i])
    } else {
      statistics <-
        add_statistic(statistics, "estimate", df$estimate[i])
      statistics <-
        add_statistic(statistics, "interval", df$ci.lb[i], "CI", "lower")
      statistics <-
        add_statistic(statistics, "interval", df$ci.ub[i], "CI", "upper")
    }
    # Add statistics to the group
    group$statistics <- statistics
    # Add the group to the groups of the coefficients groups list
    groups$groups <- append(groups$groups, list(group))
  }
  return(list(groups))
}