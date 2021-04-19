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


