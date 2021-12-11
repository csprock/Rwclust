
matrix_power <- function(x, k, ...) {
  Reduce(`%*%`, rep(list(x), k), ...)
}

matrix_summation <- function(mat_list) {
  Reduce(`+`, mat_list, accumulate = FALSE)
}

# regexpr(text = class(M), pattern = "^[a-zA-z]+Matrix$") == 1
compute_transition_matrix <- function(M) {
      M / Matrix::rowSums(M)
}


compute_kernel <- function(edgelist, mat, similarity, ...) {
  apply(
    X = edgelist,
    FUN = apply_similarity,
    MARGIN = 1,
    mat = mat, 
    similarity = similarity, 
    ...
  )
}


construct_kernel <- function(edgelist, weights, ...) {
  Matrix::sparseMatrix(
    i = edgelist[,1],
    j = edgelist[,2],
    x = weights,
    ...
  )
}




update_weights <- function(M, el, similarity, k) {
  
  M <- compute_transition_matrix(M)
  Mk <- matrix_summation(
    matrix_power(M, k, accumulate = TRUE)
  )
  
  weights <- compute_kernel(el, Mk, similarity = similarity, k = k)
  adj <- construct_kernel(el, weights, symmetric = TRUE, check = TRUE)

  return(list(weights = weights, adj = adj))

}

compute_new_weights <- function(M, el, similarity, k, iter) {

  if (!is.numeric(iter) || iter < 1) {
    stop("Invalid value for iter")
  }

  for (i in 1:iter) {
    results <- update_weights(M = M, el = el, similarity = similarity, k = k)
    M <- results$adj
  }

  return(results)
}