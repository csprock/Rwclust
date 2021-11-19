
matrix_power <- function(x, k, ...) {
  Reduce(`%*%`, rep(list(x), k), ...)
}
