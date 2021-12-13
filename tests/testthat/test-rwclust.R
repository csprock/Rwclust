test_that(
  "rwclust runs error-free with valid input", 
{
  
  df <- data.frame(
    from = c(1,2,3),
    to = c(2,3,1),
    weight = c(1,1,1)
  )

  output <- rwclust(df, iter=3, k=1)
  expect_type(output, "list")

  if (requireNamespace("igraph", quietly = TRUE)) {

    g <- igraph::graph_from_edgelist(as.matrix(df[, c(1,2)]), directed=FALSE)
    igraph::E(g)$weights <- df$weight

    output_g <- rwclust(g, iter=3, k=1)
    expect_type(output_g, "list")

  }

})
