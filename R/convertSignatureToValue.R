#' Convert Signature to Predicted Values
#'
#' This function calculates predicted values of a series based on its previous value and predicted signature changes. It starts with an initial value from a historical series and sequentially adds predicted signature changes to generate a sequence of predicted values.
#'
#' @param E Integer, the length of the series for which predictions are needed.
#' @param tau Integer, the time delay used in the system dynamics (not used in this function but typically relevant in the broader context of time series prediction).
#' @param Y Numeric vector, the original series from which the prediction starts.
#' @param i Integer, the starting index in the vector Y from which the prediction should commence.
#' @param h Integer, the horizon step for which the initial predicted value is directly obtained from Y.
#' @param predictedSignatureY Numeric vector, the predicted changes (signature) at each step used for prediction.
#'
#' @return Numeric vector containing the predicted values of the series starting from index i+h in Y and extending for E steps, adjusted by the predicted signature.
#' @export
#' @examples
#' Y <- c(1, 2, 3, 5, 8, 13, 21)
#' E <- 5
#' tau <- 1 # Example value, not used in the function
#' i <- 2
#' h <- 3
#' predictedSignatureY <- c(0.5, 1.5, 2.5, 3.5, 4.5)
#' predictedValues <- convertSignatureToValue(E, tau, Y, i, h, predictedSignatureY)
#' print(predictedValues)
convertSignatureToValue <- function(E, tau, Y, i, h, predictedSignatureY) {
  predictedY <- vector(mode = "double", length = E)
  predictedY[1] <- Y[i + h]
  for (k in 2:E) {
    predictedY[k] <- predictedY[k - 1] + predictedSignatureY[k - 1]
  }
  return(predictedY)
}
