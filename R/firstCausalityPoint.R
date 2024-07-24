#' @title First Causality Point Function
#' @description Calculates the earliest index in a time series from which causality analysis can begin,
#' based on the embedding dimension, time delay, prediction horizon, and the length of the series. It ensures
#' that there are enough data points for conducting a causality analysis without running out of data.
#' @param E An integer representing the embedding dimension, which influences the number of dimensions
#' in which the time series is reconstructed for analysis.
#' @param tau An integer representing the time delay, used in reconstructing the time series in the embedded space.
#' @param h An integer representing the prediction horizon, indicating how far ahead in the time series the predictions are aimed.
#' @param X A numeric vector representing the time series data.
#' @return An integer indicating the first index in the time series from which causality analysis is feasible without running out of data.
#' If the parameters are set such that the analysis is not possible, the function will stop and provide an error message.
#' @examples
#' time_series <- rnorm(1000) # Generate a random time series of 1000 points
#' embedding_dim <- 3 # Set embedding dimension
#' time_delay <- 2 # Set time delay
#' pred_horizon <- 1 # Set prediction horizon
#'
#' # Calculate the first causality point
#' fc_point <- firstCausalityPoint(embedding_dim, time_delay, pred_horizon, time_series)
#' print(fc_point)
#' @export
firstCausalityPoint <- function(E, tau, h, X) {
  NNSPAN <- E + 1 # Former NN | Reserves a minimum number of nearest neighbors
  CCSPAN <- (E - 1) * tau # This will remove the common coordinate NNs
  PredSPAN <- h
  FCP <- 1 + NNSPAN + CCSPAN + PredSPAN
  if (NNSPAN + CCSPAN + PredSPAN >= length(X) - CCSPAN) {
    stop("The First Point to consider for Causality does not have sufficient
         Nearest Neighbors. Please Check parameters:
         E, lag, p as well as the length of X and Y")
  } else {
    FCP <- 1 + NNSPAN + CCSPAN + PredSPAN # First Causality Point to be considered
  }
  return(FCP)
}
