#' @title Projected Nearest Neighbors Information
#' @description Extracts and returns information about the projected nearest neighbors in a time series context, specifically useful for understanding interactions in dynamic complex systems.
#' @param My Matrix of coordinates in the original space.
#' @param Dy Distance matrix, representing distances between elements in My.
#' @param SMy Matrix of signatures, capturing essential patterns in the data.
#' @param PSMy Matrix of patterns, representing characteristic configurations of the data.
#' @param timesX Index at which the projection starts.
#' @param i Index of the specific element for which information is being extracted.
#' @param h Horizon over which the projection is considered.
#' @return A list containing indices of the element, the projected times, distances, weights derived from these distances, signatures, patterns, and the coordinates of the nearest neighbors.
#' @examples
#' set.seed(123)
#' E <- 3
#' tau <- 1
#' Mx <- matrix(rnorm(300), nrow = 100)
#' My <- matrix(rnorm(300), nrow = 100)
#' Dx <- distanceMatrix(Mx, "minkowski")
#' Dy <- distanceMatrix(My, "minkowski")
#' SMx <- signatureSpace(Mx, E)
#' SMy <- signatureSpace(My, E)
#' PSMx <- patternSpace(SMx, E)
#' PSMy <- patternSpace(SMy, E)
#' CCSPAN <- (E - 1) * tau
#' NNSPAN <- E + 1
#' i <- 15
#' h <- 2
#' NNx <- pastNNsInfo(CCSPAN, NNSPAN, Mx, Dx, SMx, PSMx, i, h)
#' timesX <- NNx$times
#' projNNy <- projectedNNsInfo(My, Dy, SMy, PSMy, timesX, i, h)
#' print(projNNy)
#' @export
projectedNNsInfo <- function(My, Dy, SMy, PSMy, timesX, i, h) {
  w <- weightsRelativeToDistance(Dy[i, timesX + h])
  wPY <- list(
    "i" = i,
    "Times_Projected" = timesX + h,
    "Dists" = Dy[i, timesX + h],
    "Weights" = as.vector(w),
    "Signatures" = SMy[timesX + h, ],
    "Patterns" = PSMy[timesX + h],
    "Coordinates" = My[timesX + h, ]
  )
  return(wPY)
}

#' Compute Weights Relative to Distance
#' @description Calculates weights for each distance vector element through a normalization and exponential decay function, which emphasizes nearer points in a geometric space.
#' @param distsVec Vector of distances.
#' @return Normalized weight vector emphasizing smaller distances more significantly.
#' @noRd
# @keywords internal
# @export
weightsRelativeToDistance <- function(distsVec) {
  weights_1 <- distsVec
  w.total <- sum(weights_1)
  if (w.total == 0) {
    w.total <- 0.0001
  }
  weights_2 <- weights_1 / w.total
  w <- exp(-weights_2) / sum(exp(-weights_2))
  return(w)
}
