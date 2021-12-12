
check_edgelist <- function(x) {
  if (x[2] < x[1]) {
    return(c(x[2], x[1]))
  } else{
    return(x)
  }
}

enforce_upper_triangular <- function(el) {
  t(apply(X=el, MARGIN=1, FUN=check_edgelist))
}


check_simple <- function(el) {
    has_dupes <- apply(X=el, MARGIN=1, FUN=function(x){x[1]==x[2]})
    if (sum(has_dupes) > 0){
        stop("Self-loops are not allowed.")
    }
}

check_duplicates <- function(el) {
  if (duplicated(el) > 0) {
    stop("Duplicated / multi-edges are not allowed.")
  }
}


check_df_dims <- function(el) {
    if (ncol(el) != 3) {
        stop("Array representing edgelist must have 3 columns")
    }
}

check_simple <- function(g) {
  if (!igraph::is.simple(g)) {
    stop("Graph must be simple")
  }
}

check_undirected <- function(g) {
  if (igraph::is.directed(g)) {
    stop("Graph must be undirected")
  }
}

rwclust <- function(x, similarity="hk",...) {

    if (is.data.frame(x) || is.matrix(x)) {
        x <- as.matrix(x)
        check_df_dims(x)
        x[, c(1,2)] <- enforce_upper_triangular(x[, c(1,2)])
        check_duplicates(x)
        check_simple(x)

        adj <- Matrix::sparseMatrix(
            i = x[,1],
            j = x[,2],
            x = x[,3],
            symmetric=TRUE
        )
       
    } else if (requireNamespace("igraph", quietly = TRUE)) {

      if (igraph::is.igraph(x)) {

        check_undirected(x)
        check_simple(x)

        if (!is.null(igraph::get.vertex.attribute(x, "name"))) {
          warning("Vertex names will be converted to interger indices.")
        }
        if (!is.null(igraph::get.edge.attribute(x, "weights"))) {
          stop("Edge attribute 'weights' must be set")
        }

        adj <- igraph::as_adjacency_matrix(x, type="both", sparse=TRUE, attr="weights")
        x <- igraph::as_edgelist(x, names=FALSE)

      } else {
        stop("x must be dataframe, matrix or igraph.graph")
      }
    } else {
        stop("x must be dataframe, matrix or igraph.graph")
    }

    similarity <- switch(
        similarity,
        "hk" = hk_similarity
    )

    sharpened_weights <- compute_new_weights(adj, x, similarity = similarity, ...)

    if (igraph::is.igraph(x)){
      
      g <- igraph::graph_from_adjacency_matrix(adj)
      igraph::E(g) <- sharpened_weights$weights
      return(g)
    }

    output <- list(
        weights = sharpened_weights$weights,
        adjacency = sharpened_weights$adj
    )

    return(output)
}
