#' @title Calculate Distance Matrix
#'
#' @description This function computes the distance matrix from a given embedded matrix, `M`, using a specified metric. The distance matrix is essential for exploring the underlying structure in complex systems by quantifying the distances between different points (or states) in the embedded space. This matrix can be crucial for further analysis like clustering, nearest neighbor searches, or causality inference in complex systems.
#'
#' @param M Numeric matrix, the embedded state space matrix where each row represents a point in the reconstructed state space of a time series or any multidimensional data.
#' @param metric Character, the distance metric to be used for computing distances. Common metrics include "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#'
#' @return An object of class `dist`, representing the distance matrix of the embedded matrix `M`. This distance matrix can optionally be converted to a full matrix format if needed for subsequent analyses.
#' @export
#' @examples
#' M <- matrix(rnorm(100), nrow = 10)
#' distanceMat <- distanceMatrix(M, "euclidean")
#' print(distanceMat) # Optionally convert to a full matrix for display
distanceMatrix <- function(M, metric) {
  # d <- dist(M, metric, upper = TRUE)
  d <- as.matrix(stats::dist(M, metric, upper = T))
  return(d)
}