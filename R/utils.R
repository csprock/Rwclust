#' rwclust class constructor
#' 
#' Returns a object of class "rwclust" for use with generic summary and plotting functions. 
#' 
#' @param weights vector of double containing the edge weights
#' @param adj Matrix produced at the final iteration of the algorithm
new_rwclust <- function(weights, adj) {
    structure(weights, adjacency=adj, class="rwclust")
}