#' Compute Distance Matrix for an Embedded Matrix
#'
#' This function computes the distance matrix from a given embedded matrix, `M`, using a specified metric. The distance matrix is essential for exploring the underlying structure in complex systems by quantifying the distances between different points (or states) in the embedded space. This matrix can be crucial for further analysis like clustering, nearest neighbor searches, or causality inference in complex systems.
#'
#' @param M Numeric matrix, the embedded state space matrix where each row represents a point in the reconstructed state space of a time series or any multidimensional data.
#' @param metric Character, the distance metric to be used for computing distances. Common metrics include "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#'
#' @return An object of class `dist`, representing the distance matrix of the embedded matrix `M`. This distance matrix can optionally be converted to a full matrix format if needed for subsequent analyses.
#'
#' @export
#' @examples
#' # Assume M is an already constructed state space matrix of a time series
#' M <- matrix(rnorm(100), nrow=10)
#' distanceMat <- distanceMatrix(M, "euclidean")
#' print(distanceMat)  # Optionally convert to a full matrix for display
distanceMatrix <- function(M, metric) {
  #d <- dist(M, metric, upper = TRUE)
  d <- as.matrix(dist(M,metric, upper=T))
  return(d)
}

#' Calculate Generalized Minkowski Distance Between Two Vectors
#'
#' This function calculates the generalized Minkowski distance of order 'n' between two numeric vectors. This distance is a metric in a normed vector space which generalizes the Euclidean and Manhattan distances. It is used for various data science applications, particularly in clustering, optimization, and outlier detection in complex systems.
#'
#' @param vec1 Numeric vector, the first vector for which the distance will be calculated.
#' @param vec2 Numeric vector, the second vector for which the distance will be calculated.
#' @param n Integer, the order of the Minkowski distance. When n=2, it becomes the Euclidean distance; when n=1, it becomes the Manhattan distance.
#'
#' @return Numeric, the computed Minkowski distance between the two vectors.
#'
#' @export
#' @examples
#' vec1 <- c(1, 2, 3)
#' vec2 <- c(4, 5, 6)
#' n <- 2
#' distance <- metricDistance(vec1, vec2, n)
#' print(distance)
metricDistance <- function(vec1, vec2, n) {
  res <- as.numeric(vec1 - vec2)
  distance <- (sum(abs(res)^n))^(1/n)
  return(distance)
}

#' Calculate Distances Between a Reference Point and Multiple Candidates
#'
#' This function applies the 'metricDistance' function to calculate distances from a given reference point to each row in a matrix of candidate nearest neighbors. It is particularly useful in the situation between matrix and a vector
#'
#' @param point Numeric vector, the reference point from which distances are calculated.
#' @param candidateNNs Matrix, rows represent candidate points to which distance from the reference point is calculated.
#' @param n Integer, the order of the Minkowski distance to use.
#'
#' @return Numeric vector, distances from the reference point to each of the candidate points.
#'
#' @export
#' @examples
#' point <- c(1, 2, 3)
#' candidateNNs <- matrix(c(4, 5, 6, 7, 8, 9), nrow=2, byrow=TRUE)
#' n <- 2
#' distances <- distanceVector(point, candidateNNs, n)
#' print(distances)
distanceVector <- function(point, candidateNNs, n) {
  apply(X = candidateNNs, MARGIN = 1, FUN = metricDistance, vec2 = point, n = n)
}

