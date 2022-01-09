#' Example Graph 1
#' 
#' Second demonstration test graph used in the original paper by Harel and Koren.
#' 
#' @source \url{https://link.springer.com/chapter/10.1007/3-540-45294-X_3}
#' 
#' @references
#' Harel, David, and Yehuda Koren. "On clustering using random walks." 
#' International Conference on Foundations of Software Technology 
#' and Theoretical Computer Science. Springer, Berlin, Heidelberg, 2001.
#' 
#' @examples
#' \dontrun{
#' data(example1, package="Rwclust")
#' }
#' 
#' @format A data frame with three columns representing a weighted graph. Each
#' row represents an edge with a weight:
#' \describe{
#'  \item{from}{An integer vertex id}
#'  \item{to}{An integer vertex id}
#'  \item{weight}{A double representing the edge weight}
#' }
"example1"


#' Example Graph 2
#' 
#' Second demonstration test graph used in the original paper by Harel and Koren.
#' 
#' @source \url{https://link.springer.com/chapter/10.1007/3-540-45294-X_3}
#' 
#' @references
#' Harel, David, and Yehuda Koren. "On clustering using random walks." 
#' International Conference on Foundations of Software Technology 
#' and Theoretical Computer Science. Springer, Berlin, Heidelberg, 2001.
#' 
#' @examples
#' \dontrun{
#'  data(example2, package="Rwclust")
#' }
#' 
#' @format A data frame with three columns representing a weighted graph. Each
#' row represents an edge with a weight.
#' \describe{
#'  \item{from}{An integer vertex id}
#'  \item{to}{An integer vertex id}
#'  \item{weight}{A double representing the edge weight}
#' }
"example2"