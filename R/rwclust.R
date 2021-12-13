
rwclust <- function(x, similarity="hk", iter, k) {

    if (is.data.frame(x) || is.matrix(x)) {

        x <- format_and_check_dataframe(x)

        adj <- Matrix::sparseMatrix(
            i = x[,1],
            j = x[,2],
            x = x[,3],
            symmetric=TRUE
        )

        return(rwclust_(adj, x, similarity, iter, k))
        
    } else if (requireNamespace("igraph", quietly = TRUE)) {

      if (igraph::is.igraph(x)) {

        check_igraph(x)

        adj <- igraph::as_adjacency_matrix(x, type="both", sparse=TRUE, attr="weights")
        el <- igraph::as_edgelist(x, names=FALSE)

        return(rwclust_(adj, el, similarity, iter, k))

      } else {
        stop("x must be dataframe, matrix or igraph.graph")
      }

     } else {
        stop("x must be a 3-column data frame or matrix")
    }
}


rwclust_ <- function(adj, edgelist, similarity="hk", iter, k) {

    similarity <- switch(
        similarity,
        "hk" = hk_similarity
    )

    sharpened_weights <- compute_new_weights(adj, edgelist, similarity, k, iter)

    output <- list(
        weights = sharpened_weights$weights,
        adjacency = sharpened_weights$adj
    )

    return(output)
}
