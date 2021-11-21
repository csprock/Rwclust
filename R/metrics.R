
l1_norm <- function(x,y) {sum(abs(x - y))}

hk_similarity <- function(x, y, k) {
    exp(2*k - l1_norm(x, y) - 1)
}


apply_similarity<- function(idx, mat, similarity, ...) {
  similarity(mat[idx[1]], mat[idx[2]], ...)
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
