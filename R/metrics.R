
l1_norm <- function(x,y) {sum(abs(x - y))}

hk_similarity <- function(x, y, k) {
    exp(2*k - l1_norm(x, y)) - 1
}
