## code to prepare `DATASET` dataset goes here

# library(Matrix)

# convert_to_Matrix <- function(data) {
#   n <- max(data$from, data$to)
#   M <- Matrix(0, nrow=n, ncol=n)
#   for (k in 1:nrow(data)) {
#     M[data$from[k], data$to[k]] <- data$weight[k]
#     M[data$to[k], data$from[k]] <- data$weight[k]
#   }
#   return(M)
# }

example1 <- read.csv("data-raw/example1.csv", header=TRUE)
example2 <- read.csv("data-raw/example2.csv", header=TRUE)

usethis::use_data(example1, example2, overwrite = TRUE)
