#' Predict the signatures and patterns for a complex system
#'
#' This function predicts the signature and pattern vectors for a given state based on 
#' projections and weights derived from neural network outputs within a complex system. 
#' The predictions adjust according to a specified zero tolerance level to manage sparsity.
#'
#' @title Predict Signature and Pattern Vectors
#' @description Uses neural network outputs to predict the state signatures and patterns 
#' in a complex system. Adjusts for sparsity using zero tolerance.
#' @param E Integer, the embedding dimension of the system, indicating the length of the 
#' signature vector minus one.
#' @param projNNy A list containing two elements: `Signatures`, a matrix where each column 
#' represents a component in the signature vector across different observations, and 
#' `Weights`, a numeric vector representing the weights associated with each observation.
#' @param zeroTolerance A numeric value used to determine the sparsity threshold in the 
#' signature matrix. Default is set to (E+1)/2.
#' @return A dataframe with two columns: `predictedSignatureY` which contains the predicted 
#' signature vector, and `predictedPatternY` which contains the corresponding pattern vector.
#' @examples
#' set.seed(123)
#' E <- 3
#' tau <- 1
#' Mx <- matrix(rnorm(200), nrow=20)
#' My <- matrix(rnorm(200), nrow=20)
#' Dx <- distanceMatrix(Mx,"minkowski")
#' Dy <- distanceMatrix(My,"minkowski")
#' SMx <- signatureSpace(Mx,E)
#' SMy <- signatureSpace(My,E)
#' PSMx <- patternSpace(SMx,E)
#' PSMy <- patternSpace(SMy,E)
#' CCSPAN <- (E-1)*tau
#' NNSPAN <- E+1
#' i <- 15
#' h <- 2
#' NNx <- pastNNsInfo(CCSPAN, NNSPAN, Mx, Dx, SMx, PSMx, i, h)
#' timesX <- NNx$times
#' projNNy <- projectedNNsInfo(My, Dy, SMy, PSMy, timesX, i, h)
#' predicted <- predictionY(E, projNNy)
#' print(predicted)
#' @export
predictionY <- function(E,projNNy,zeroTolerance=(E+1)/2) {
  if (E >= 3) {
    predictedSignatureY <- rep(0,E-1)
    for (part in 1:length(predictedSignatureY)) {
      predictedSignatureY[part] <- ifelse(length(which(projNNy$Signatures[,part]==0))>zeroTolerance,
                                          0,
                                          sum(projNNy$Signatures[,part]*projNNy$Weights))
    }
  } else {
    predictedSignatureY <- ifelse(length(which(projNNy$Signatures==0))>zeroTolerance,
                                  0,
                                  sum(projNNy$Signatures*projNNy$Weights))
  }
  predictedPatternY <- patternVectorDifference(predictedSignatureY)
  return(data.frame(predictedSignatureY=predictedSignatureY,
                    predictedPatternY=predictedPatternY))
}