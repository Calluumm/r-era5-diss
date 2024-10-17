# EOF Function
perform_eof <- function(data_matrix) {
  eof_result <- svd(data_matrix)
  return(eof_result)
}

# PCA Function
perform_pca <- function(data_matrix) {
  pca_result <- prcomp(data_matrix, scale. = TRUE)
  return(pca_result)
}

