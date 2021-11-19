
matrix_power <- function(x, k, ...) {
  Reduce(`%*%`, rep(list(x), k), ...)
}

matrix_summation <- function(mat_list) {
  Reduce(`+`, mat_list, accumulate = FALSE)
}

compute_transition_matrix <- function(M) {
    M / Matrix::rowSums(M)
}