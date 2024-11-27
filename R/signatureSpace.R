#' @title Create Signature Space
#' @description This function computes the signature space of a matrix obtained from a time series' state space by calculating the successive differences of each embedded vector. The signature space reveals changes between successive points in the time series, capturing dynamics that are crucial for understanding complex system behaviors.
#'
#' @param M Matrix, the embedded state space matrix where each row is a point in the reconstructed state space of the time series.
#' @param E Integer, the embedding dimension used to create the matrix M.
#'
#' @return Matrix where each row represents the vector of differences between successive elements of the corresponding row in matrix M. The orientation of the matrix may vary depending on the embedding dimension.
#'
#' @export
#' @examples
#' data(climate_indices)
#' ts <- climate_indices$AO
#' E <- 3
#' tau <- 1
#' stateSpaceMatrix <- stateSpace(ts, E, tau)
#' signatureMatrix <- signatureSpace(stateSpaceMatrix, E)
#' print(signatureMatrix)
signatureSpace <- function(M, E) {
  if (E < 2) {
    stop("Please input the correct E")
  } else if (E == 2) {
    SM <- as.matrix(apply(M, 1, signatureVectorDifference))
  } else if (E >= 3) {
    SM <- t(as.matrix(apply(M, 1, signatureVectorDifference)))
  }
  return(SM)
}


signatureVectorDifference <- function(vec) {
  s.vec <- (vec[-1] - vec[-length(vec)]) # Differences between successive elements
  return(s.vec)
}
