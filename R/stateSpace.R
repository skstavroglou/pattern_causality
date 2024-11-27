#' @title Create State Space
#' @description This function reconstructs the state space of a given time series by creating a delay embedding matrix. Each row in the matrix represents a point in the reconstructed state space, with embeddings based on specified lag (tau) and dimension (E). This approach is essential for analyzing the dynamics of complex systems, as it reveals underlying structures and patterns that are not immediately apparent in the raw time series data.
#'
#' @param ts Numeric vector, the original time series data from which the state space is to be reconstructed.
#' @param E Integer, the embedding dimension, which determines the number of delayed copies of the time series to include in each row of the matrix.
#' @param tau Integer, the time delay between each copy in the embedding, indicating the spacing in time steps between elements in each embedded vector.
#'
#' @return A matrix where each row corresponds to an embedded vector, representing a point in the reconstructed state space of the time series. Rows with insufficient data (due to the delay embedding reaching beyond the end of the time series) contain NA values.
#'
#' @export
#' @examples
#' ts <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' E <- 3
#' tau <- 1
#' stateSpaceMatrix <- stateSpace(ts, E, tau)
#' print(stateSpaceMatrix)
stateSpace <- function(ts, E, tau) {
  M <- matrix(NA, length(ts) - E + 1 - tau + 1, E)
  for (i in 1:nrow(M)) {
    M[i, ] <- ts[seq(from = i, to = i + tau * (E - 1), by = tau)]
    if (anyNA(M[i, ])) {
      M[i, ] <- rep(NA, E)
    }
  }
  return(M)
}
