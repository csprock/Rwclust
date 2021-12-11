
check_edgelist <- function(x) {
  if (x[2] < x[1]) {
    return(c(x[2], x[1]))
  } else{
    return(x)
  }
}

enforce_upper_triangular <- function(el) {
  t(apply(X=temp, MARGIN=1, FUN=check_edgelist))
}


check_simple <- function(el) {
  has_dupes <- apply(X=temp, MARGIN=1, FUN=function(x){x[1]==x[2]})
  if (sum(has_dupes) > 0){
    stop("Self-loops are not allowed.")
  }
}

check_duplicates <- function(el) {
  if (duplicated(el) > 0) {
    stop("Duplicated / multi-edges are not allowed.")
  }
}



sharpen_weights <- function(g, similarity, k, iter) {

    similarity <- switch(
        similarity,
        "hk" = hk_similarity
    )

    adj <- igraph::as_adjacency_matrix(g, attr = "weights", type="both", sparse = TRUE)
    el <- igraph::as_edgelist(g, names = FALSE)

    sharpened_weights <- compute_new_weights(adj, el, similarity, k, iter)

    new_graph <- g
    E(new_graph)$weights <- sharpened_weights$weights

    output <- list(
        g = new_graph,
        weights = sharpened_weights$weights,
        adj = sharpened_weights$adj
    )

    return(output)
}