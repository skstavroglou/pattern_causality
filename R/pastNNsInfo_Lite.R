#' @title Finding Nearest Neighbors and Keeping their Topological Information (Lite Version)
#' @description This function is a simplified version of `pastNNsInfo`, identifying the nearest neighbors of a given point in a time series. It returns detailed information about these neighbors, including their times, distances, signatures, patterns, and coordinates, without excluding common coordinate vectors and horizon.
#' @param CCSPAN Integer, the span of common coordinates to exclude from the nearest neighbor search (not used in Lite version).
#' @param NNSPAN Integer, the number of nearest neighbors to consider for the analysis.
#' @param Mx Matrix, the main matrix representing the state space of the system.
#' @param Dx Numeric matrix, containing distances between points in the state space.
#' @param SMx Matrix, containing signatures of the state space.
#' @param PSMx Matrix, containing patterns derived from the signatures.
#' @param i Integer, the current index in time series data for which nearest neighbors are being considered.
#' @param h Integer, the horizon beyond which data is not considered in the nearest neighbor search (not used in Lite version).
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
#' Mx <- matrix(rnorm(200), nrow=20)
#' CCSPAN <- (E-1)*tau
#' NNSPAN <- E+1
#' i = 15
#' h <- 2
#' Dx <- distanceVector(point = Mx[i,], candidateNNs = Mx[1:(i-CCSPAN-h),],n = 2)
#' SMx <- signatureSpace(Mx,E)
#' PSMx <- patternSpace(SMx,E)
#' neighborsInfoLite <- pastNNsInfo_Lite(CCSPAN, NNSPAN, Mx, Dx, SMx, PSMx, i, h)
#' print(neighborsInfoLite)
#' @export
pastNNsInfo_Lite <- function(CCSPAN,NNSPAN,Mx,Dx,SMx,PSMx,i,h) {
  # REMOVE COMMON COORDINATE VECTORS + horizon FROM CANDIDATE NNs
  candidateNNs <- Dx
  # NEAREST NEIGHBORS OF Y TO PREDICT IN X 
  times <- as.numeric(names(candidateNNs[order(candidateNNs)])[1:NNSPAN])
  dists <- candidateNNs[order(candidateNNs)][1:NNSPAN]
  #cat(paste("At Loop time (i):", i, ".."))
  #cat(paste("times:", times, ".."))
  #cat(paste("dists:", dists, ".."))
  #= THEIR SIGNATURES
  signatures = SMx[times,]
  #= DELIVERABLE =#
  thePast <- list("i"=i,"times"=times,"dists"=dists,
                  "signatures"=signatures,"patterns"=PSMx[times],
                  "coordinates"=Mx[times,])
  return(thePast)
}
