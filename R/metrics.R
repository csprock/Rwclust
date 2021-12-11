
l1_norm <- function(x,y) {sum(abs(x - y))}

hk_similarity <- function(x, y, k) {
    exp(2*k - l1_norm(x, y)) - 1
}


apply_similarity<- function(idx, mat, similarity, ...) {
  similarity(mat[idx[1],], mat[idx[2],], ...)
}
