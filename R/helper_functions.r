
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
    as_tibble(rownames = "name1") %>%
    pivot_longer(-name1, names_to = "name2", values_to = "value") %>%
    filter(!is.na(value) & name1 != name2)
 
  return(df)   
}


