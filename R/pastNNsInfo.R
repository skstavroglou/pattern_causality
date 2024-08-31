#' @title Finding Nearest Neighbors and Keeping their Topological Information
#' @description This function identifies the nearest neighbors of a given point in a time series, excluding common coordinate vectors and a specified horizon from the candidate nearest neighbors. It returns detailed information about these neighbors, including their times, distances, signatures, patterns, and coordinates.
#' @param CCSPAN Integer, the span of common coordinates to exclude from the nearest neighbor search.
#' @param NNSPAN Integer, the number of nearest neighbors to consider for the analysis.
#' @param Mx Matrix, the main matrix representing the state space of the system.
#' @param Dx Numeric matrix, containing distances between points in the state space.
#' @param SMx Matrix, containing signatures of the state space.
#' @param PSMx Matrix, containing patterns derived from the signatures.
#' @param i Integer, the current index in time series data for which nearest neighbors are being considered.
#' @param h Integer, the horizon beyond which data is not considered in the nearest neighbor search.
#' @return A list containing:
#'   - `i`: The current index in time series data.
#'   - `times`: The times of the nearest neighbors.
#'   - `dists`: The distances to the nearest neighbors.
#'   - `signatures`: The signatures of the nearest neighbors.
#'   - `patterns`: The patterns of the nearest neighbors.
#'   - `coordinates`: The coordinates of the nearest neighbors.
#' @examples
#' # Generate random data for demonstration
#' set.seed(123)
#' E <- 3
#' tau <- 1
#' Mx <- matrix(rnorm(300), nrow = 100)
#' Dx <- distanceMatrix(Mx, "minkowski")
#' SMx <- signatureSpace(Mx, E)
#' PSMx <- patternSpace(SMx, E)
#' CCSPAN <- (E - 1) * tau
#' NNSPAN <- E + 1
#' i <- 15
#' h <- 2
#' neighborsInfo <- pastNNsInfo(CCSPAN, NNSPAN, Mx, Dx, SMx, PSMx, i, h)
#' print(neighborsInfo)
#' @export
pastNNsInfo <- function(CCSPAN, NNSPAN, Mx, Dx, SMx, PSMx, i, h) {
  candidateNNs <- Dx[i, 1:(i - CCSPAN - h)]
  times <- as.numeric(names(candidateNNs[order(candidateNNs)])[1:NNSPAN])
  dists <- candidateNNs[order(candidateNNs)][1:NNSPAN]
  signatures <- SMx[times, ]
  thePast <- list(
    "i" = i, "times" = times, "dists" = dists,
    "signatures" = signatures, "patterns" = PSMx[times],
    "coordinates" = Mx[times, ]
  )
  return(thePast)
}
