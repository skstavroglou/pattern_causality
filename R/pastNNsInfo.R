#' Retrieve Information on Past Nearest Neighbors
#'
#' This function extracts and organizes information about the nearest neighbors of a given point in time series data,
#' utilizing past data up to a specified horizon. It operates within a framework that incorporates multiple matrices
#' representing different attributes of the system's state space, such as distances, signatures, and patterns.
#'
#' @param CCSPAN Integer, the span of common coordinates to exclude in the nearest neighbor search.
#' @param NNSPAN Integer, the number of nearest neighbors to consider for the analysis.
#' @param Mx Matrix, the main matrix representing the state space of the system.
#' @param Dx Numeric matrix, containing distances between points in the state space.
#' @param SMx Matrix, containing signatures of the state space.
#' @param PSMx Matrix, containing patterns derived from the signatures.
#' @param i Integer, the current index in time series data for which nearest neighbors are being considered.
#' @param h Integer, the horizon beyond which data is not considered in the nearest neighbor search.
#'
#' @return A list containing indices, times, distances, signatures, patterns, and original coordinates of the nearest
#' neighbors from past data, useful for subsequent analysis and prediction in the context of complex systems.
#'
#' @export
#' @examples
#' # Random data generation for demonstration
#' set.seed(123)
#' Mx <- matrix(rnorm(200), nrow=20)
#' Dx <- as.matrix(dist(Mx))
#' SMx <- apply(Mx, 1, function(x) diff(x))
#' PSMx <- apply(SMx, 1, function(x) ifelse(x > 0, 1, ifelse(x < 0, -1, 0)))
#' CCSPAN <- 5
#' NNSPAN <- 3
#' i <- 15
#' h <- 2
#' neighborsInfo <- pastNNsInfo(CCSPAN, NNSPAN, Mx, Dx, SMx, PSMx, i, h)
#' print(neighborsInfo)
pastNNsInfo <- function(CCSPAN, NNSPAN, Mx, Dx, SMx, PSMx, i, h) {
  # REMOVE COMMON COORDINATE VECTORS + horizon FROM CANDIDATE NNs
  candidateNNs <- Dx[i, 1:(i - CCSPAN - h)]
  # NEAREST NEIGHBORS OF Y TO PREDICT IN X 
  times <- as.numeric(names(candidateNNs[order(candidateNNs)])[1:NNSPAN])
  dists <- candidateNNs[order(candidateNNs)][1:NNSPAN]
  # THEIR SIGNATURES
  signatures <- SMx[times, ]
  # DELIVERABLE
  thePast <- list(
    "i" = i,
    "times" = times,
    "dists" = dists,
    "signatures" = signatures,
    "patterns" = PSMx[times],
    "coordinates" = Mx[times, ]
  )
  return(thePast)
}
