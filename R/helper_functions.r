
tidy_matrix <- function(m) {
  
  # Check whether there are row and column names
  if (!length(rownames(m)) > 0) {
    stop("Matrix has no row names.")
  }
  
  if (!length(colnames(m)) > 0) {
    stop("Matrix has no column names.")
  }
  
  if (sum(rownames(m) == colnames(m)) != length(rownames(m))) {
    stop("Matrix row and column names do not match.")
  }
  
  # Remove one half of the matrix because these are duplicate values
  m[lower.tri(m)] <- NA
  
  # Tidy the matrix into a data frame
  df <- m %>%
    as.matrix() %>%
    tibble::as_tibble(rownames = "row") %>%
    tidyr::pivot_longer(-row, names_to = "column", values_to = "value") %>%
    dplyr::mutate(
        row = as.numeric(row), 
        column = readr::parse_number(column)
      ) %>%
    dplyr::filter(row != column)
 
  return(df)   
}


