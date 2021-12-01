
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