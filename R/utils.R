#' rwclust class constructor
#'
#' Returns a object of class "rwclust" for use with generic summary and plotting functions.
#'
#' @param weights vector of double containing the edge weights
#' @param adj Matrix produced at the final iteration of the algorithm
new_rwclust <- function(weights, adj) {
    structure(weights, adjacency=adj, class="rwclust")
}

#' Generic plotting for rwclust object
#'
#' Generic function for plotting the distribution of weights. Calls \code{hist} under the hood.
#'
#' @param x rwclust object
#' @param cutoff optional numeric, will plot the cutoff value as a vertical line
#' @param ... additional graphical parameters passed to the \code{hist} function
# 
#' @importFrom checkmate assert_number
#' @export
plot.rwclust <- function(x, cutoff=NULL, ...) {
    weights <- unclass(x)
    hist(weights, main="Distribution of Edge Weights", xlab="Edge Weights", ...)
    if (!is.null(cutoff)) {
        assert_number(cutoff)
        abline(v=cutoff, col="red")
    }
}
