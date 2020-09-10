#' Helper functions in tidystats
#' 

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
  m[lower.tri(m, diag = TRUE)] <- NA
  
  # Tidy the matrix into a data frame
  df <- m %>%
    as.matrix() %>%
    tibble::as_tibble(rownames = "name1") %>%
    tidyr::pivot_longer(-name1, names_to = "name2", values_to = "value") %>%
    dplyr::filter(!is.na(value))
 
  return(df)   
}


