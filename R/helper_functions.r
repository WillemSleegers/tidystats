#' Helper functions in tidystats
#'
#' Functions used under the hood in the tidystats package.

#' @describeIn helper_functions
#' Function to convert matrix objects to a tidy data frame.
#'
#' @param m A matrix.
#'
#' @keywords internal
tidy_matrix <- function(m, symmetric = TRUE) {
  if (!length(rownames(m)) > 0) {
    stop("Matrix has no row names.")
  }

  if (!length(colnames(m)) > 0) {
    stop("Matrix has no column names.")
  }

  if (symmetric) {
    if (sum(rownames(m) == colnames(m)) != length(rownames(m))) {
      stop("Matrix row and column names do not match.")
    }
  }

  # Remove the diagonal and duplicate values in case of a symmetric matrix
  if (symmetric) {
    m[lower.tri(m, diag = TRUE)] <- NA
  }

  m_mat <- as.matrix(m)
  df_wide <- data.frame(name1 = rownames(m_mat), as.data.frame(m_mat),
    check.names = FALSE, row.names = NULL
  )
  col_names <- setdiff(names(df_wide), "name1")
  rows <- lapply(col_names, function(cn) {
    data.frame(name1 = df_wide$name1, name2 = cn, value = df_wide[[cn]],
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  df <- df[!is.na(df$value), ]

  return(df)
}

# Tidying -----------------------------------------------------------------

#' @describeIn helper_functions
#' Function to add a statistic to list. It helps create the list and ignores
#' NULL values.
#'
#' @keywords internal
add_statistic <- function(list, name, value, symbol = NULL, subscript = NULL,
                          interval = NULL, level = NULL, lower = NULL,
                          upper = NULL) {
  if (is_blank(value)) {
    return(list)
  }

  new_list <- list()
  new_list$name <- name

  if (!is_blank(symbol)) new_list$symbol <- symbol
  if (!is_blank(subscript)) new_list$subscript <- subscript

  new_list$value <- value

  if (
    !is_blank(level) &&
      !is_blank(interval) &&
      !is_blank(lower) &&
      !is_blank(upper)
  ) {
    new_list$interval <- interval
    new_list$level <- level
    new_list$lower <- lower
    new_list$upper <- upper
  }

  list <- append(list, list(new_list))

  return(list)
}

add_attribute <- function(list, object, attribute) {
  value <- attr(object, attribute)
  if (!is.null(value)) {
    list[attribute] <- value
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

is_blank <- function(x) {
  return(
    is.null(x) || is.na(x)
  )
}

# String helpers ----------------------------------------------------------

remove_string <- function(x, pattern) sub(pattern, "", x, perl = TRUE)

detect_string <- function(x, pattern) grepl(pattern, x, perl = TRUE)

extract_string <- function(x, pattern) {
  m <- regexpr(pattern, x, perl = TRUE)
  ifelse(m == -1L, NA_character_, regmatches(x, m))
}

# Data frame helpers ------------------------------------------------------

dots_to_names <- function(...) as.character(substitute(list(...))[-1])

group_names <- function(data) setdiff(names(attr(data, "groups")), ".rows")

stack_rows <- function(lst) {
  all_names <- unique(unlist(lapply(lst, names)))
  rows <- lapply(lst, function(x) {
    x[setdiff(all_names, names(x))] <- NA
    as.data.frame(x[all_names], stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Symbols -----------------------------------------------------------------

#' @describeIn helper_functions
#' Function to return symbols in ASCII.
#'
#' @keywords internal
symbol <- function(
    x = c(
      "alpha",
      "chi_squared",
      "delta",
      "guttmans_lambda",
      "K_squared",
      "lambda",
      "p_hat",
      "R_squared",
      "sigma",
      "t_squared",
      "tau"
    )) {
  symbols <- c(
    "alpha"          = intToUtf8(0x03b1),
    "chi_squared"    = paste0(intToUtf8(0x03c7), intToUtf8(0x00b2)),
    "delta"          = intToUtf8(0x03b4),
    "guttmans_lambda" = paste("Guttman's", intToUtf8(0x03bb)),
    "K_squared"      = paste0("K", intToUtf8(0x00b2)),
    "lambda"         = intToUtf8(0x03bb),
    "p_hat"          = paste0("p", intToUtf8(0x0302)),
    "R_hat"          = paste0("R", intToUtf8(0x0302)),
    "R_squared"      = paste0("R", intToUtf8(0x00b2)),
    "sigma"          = intToUtf8(0x03a3),
    "t_squared"      = paste0("t", intToUtf8(0x00b2)),
    "tau"            = intToUtf8(0x03c4)
  )
  unname(symbols[x])
}
