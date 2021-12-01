
matrix_power <- function(x, k, ...) {
  Reduce(`%*%`, rep(list(x), k), ...)
}

matrix_summation <- function(mat_list) {
  Reduce(`+`, mat_list, accumulate = FALSE)
}

compute_transition_matrix <- function(M) {
    if (is.matrix(M)) {
      return(M / apply(X = M, MARGIN = 1, FUN = sum))
    } else if (regexpr(text = class(M), pattern = "^[a-zA-z]+Matrix$") == 1) {
      return(M / Matrix::rowSums(M))
    } else {
      stop("Must supply this function with matrix")
    }
}

