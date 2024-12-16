#' Predict Signature and Pattern Vectors
#' 
#' @title Signature and Pattern Vector Prediction
#' @description Predicts signature and pattern vectors for a given state based on 
#' neural network projections and weights. Implements adaptive sparsity handling 
#' through zero tolerance thresholds.
#'
#' @details
#' The function implements these prediction steps:
#' \itemize{
#'   \item Weighted signature prediction using neural network outputs
#'   \item Sparsity handling through zero tolerance thresholds
#'   \item Pattern vector computation from predicted signatures
#' }
#'
#' @section Related Packages:
#' \itemize{
#'   \item \pkg{forecast}: Time series prediction methods
#'   \item \pkg{keras}: Neural network implementations
#'   \item \pkg{nnet}: Neural network tools
#' }
#'
#' @param projectedNN A list containing:
#'   \itemize{
#'     \item Signatures: Matrix of signature components
#'     \item Weights: Vector of observation weights
#'   }
#' @param zeroTolerance Numeric; sparsity threshold (default: E-1)
#'
#' @return An object of class "pc_prediction" containing:
#' \itemize{
#'   \item signature: Predicted signature vector
#'   \item pattern: Corresponding pattern vector
#'   \item parameters: List of prediction parameters
#' }
#'
#' @keywords internal
#' @noRd
predictionY <- function(projectedNN, zeroTolerance = NULL) {
  # Extract embedding dimension
  E <- ncol(projectedNN$signatures) + 1
  
  # Set default zero tolerance
  if(is.null(zeroTolerance)) {
    zeroTolerance <- E - 1
  }
  
  # Compute predicted signature
  if (E >= 3) {
    predictedSignatureY <- rep(NA_real_, E - 1)
    for (part in seq_len(E - 1)) {
      zero_count <- sum(projectedNN$signatures[, part] == 0)
      predictedSignatureY[part] <- if(zero_count > zeroTolerance) {
        0
      } else {
        sum(projectedNN$signatures[, part] * projectedNN$weights)
      }
    }
  } else {
    zero_count <- sum(projectedNN$signatures == 0)
    predictedSignatureY <- if(zero_count > zeroTolerance) {
      0
    } else {
      sum(projectedNN$signatures * projectedNN$weights)
    }
  }
  
  # Compute pattern vector
  predictedPatternY <- patternVectorDifference(predictedSignatureY)
  
  # Create and return pc_prediction object
  structure(
    list(
      signature = predictedSignatureY,
      pattern = predictedPatternY,
      parameters = list(
        E = E,
        zeroTolerance = zeroTolerance
      )
    ),
    class = "pc_prediction"
  )
}
