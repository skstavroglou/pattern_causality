#' Convert Signature to Value
#'
#' @description
#' Internal function that converts predicted signature changes into actual values
#' based on the current value and embedding parameters.
#'
#' @param E Integer, embedding dimension
#' @param tau Integer, time delay
#' @param Y Numeric vector, original time series
#' @param i Integer, current time index
#' @param h Integer, prediction horizon
#' @param predictedSignatureY Numeric vector of length E-1, predicted signature changes
#'
#' @return Numeric vector of length E containing predicted values
#'
#' @keywords internal
#' @noRd
convertSignatureToValue <- function(E, tau, Y, i, h, predictedSignatureY) {
  # Input validation with more informative error messages
  if(!is.numeric(E) || E <= 0 || E != round(E)) {
    stop("E must be a positive integer", call. = FALSE)
  }
  if(!is.numeric(tau) || tau <= 0 || tau != round(tau)) {
    stop("tau must be a positive integer", call. = FALSE)
  }
  if(!is.numeric(Y)) {
    stop("Y must be a numeric vector", call. = FALSE)
  }
  if(!is.numeric(i) || i < 1 || i != round(i)) {
    stop("i must be a positive integer", call. = FALSE)
  }
  if(!is.numeric(h) || h < 0 || h != round(h)) {
    stop("h must be a non-negative integer", call. = FALSE)
  }
  if(!is.numeric(predictedSignatureY)) {
    stop("predictedSignatureY must be a numeric vector", call. = FALSE)
  }
  if(length(predictedSignatureY) != E-1) {
    stop(sprintf("predictedSignatureY must have length %d (E-1)", E-1), call. = FALSE)
  }
  
  # Pre-allocate output vector for efficiency
  predictedY <- numeric(E)
  predictedY[1] <- Y[i + h]
  
  # Vectorized calculation instead of loop
  predictedY[2:E] <- predictedY[1] + cumsum(predictedSignatureY)
  
  return(predictedY)
}
