## code to prepare `DATASET` dataset goes here
library(Matrix)

convert_to_Matrix <- function(data) {
  n <- max(data$from, data$to)
  M <- Matrix(0, nrow=n, ncol=n)
  for (k in 1:nrow(data)) {
    M[data$from[k], data$to[k]] <- data$weight[k]
  }
  return(M)
}

graph1_data <- read.csv("data-raw/example1.csv", header=TRUE)
graph1_data <- read.csv("data-raw/example2.csv", header=TRUE)

example1_mat <- convert_to_Matrix(graph1_data)
example2_mat <- convert_to_Matrix(graph1_data)

usethis::use_data(example1_mat, example2_mat, overwrite = TRUE)
