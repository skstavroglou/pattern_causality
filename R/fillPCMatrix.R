#' Fill Pattern Causality Matrix with Causality Strengths
#'
#' This function calculates and fills the causality strengths between predicted and real patterns and signatures
#' for a complex system analysis. It evaluates the accuracy of predictions and computes the strength of causal
#' relationships based on norm vectors and optionally weights these strengths using the error function (erf).
#'
#' @title Calculate and Record Causality Strengths
#' @description Computes the causality strengths based on the comparison between predicted and real patterns
#' and signatures in a system's dynamic model. It applies a normalization function to measure the intensity of
#' causal influences and uses an error function for weighting if required.
#' @param weighted Logical, if TRUE, the causality strength is calculated using the error function for normalization,
#' otherwise a binary indication (1 for accurate prediction and 0 otherwise) is used.
#' @param predictedPatternY Numeric vector, the predicted pattern of variable Y at a future time step.
#' @param realPatternY Numeric vector, the actual observed pattern of variable Y at the same future time step.
#' @param predictedSignatureY Numeric vector, the predicted signature vector derived from the system model for Y.
#' @param realSignatureY Numeric vector, the actual observed signature vector for Y.
#' @param patternX Numeric vector, the current observed pattern of variable X, used as the basis for prediction.
#' @param signatureX Numeric vector, the current observed signature vector of variable X.
#' @return A dataframe with two columns: 'real' and 'predicted', representing the real and predicted causality strengths.
#' @export
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
#' pSY <- predictionY(E,projNNy,zeroTolerance=E-1)$predictedSignatureY
#' pPY <- predictionY(E,projNNy,zeroTolerance=E-1)$predictedPatternY[1]
#' rSY <- SMy[(i+h),]
#' rPY <- PSMy[i+h]
#' signatureX <- SMx[i,]
#' patternX <- PSMx[i,]
#' weighted=0
#' pc <- fillPCMatrix(weighted, pPY, rPY, pSY, rSY, patternX, signatureX)
#' @export
fillPCMatrix <- function(weighted,predictedPatternY,realPatternY,predictedSignatureY,realSignatureY,patternX,signatureX) {
  if (!anyNA(c(predictedPatternY,realPatternY,patternX))) {
    if (length(predictedPatternY)>0) {
      if (length(patternX)>0) {
        if (predictedPatternY==realPatternY) { # IF PREDICTION IS ACCURATE PROCCEED
          predictedCausalityStrength <- ifelse(weighted,erf(norm_vec(predictedSignatureY)/norm_vec(signatureX)),1)
          realCausalityStrength <- ifelse(weighted,erf(norm_vec(realSignatureY)/norm_vec(signatureX)),1)
          #predictedCausalityStrength <- ifelse(predictedCausalityStrength==0,1,predictedCausalityStrength)
          #predictedCausalityStrength <- ifelse(is.nan(predictedCausalityStrength),1,predictedCausalityStrength)
          #realCausalityStrength <- ifelse(realCausalityStrength==0,1,realCausalityStrength)
          #realCausalityStrength <- ifelse(is.nan(realCausalityStrength),1,realCausalityStrength)
          #= THIS IS THE LATEST STANDARD AS OF 27 JUNE 2020
          #predictedCausalityStrength <- 1
          #realCausalityStrength <- 1
        } else {
          predictedCausalityStrength <- 0
          realCausalityStrength <- 0
        }
      } else {
        stop("The length of the causal pattern of X is ZERO")
      }
    } else {
      stop("The length of the predicted pattern of Y is ZERO")
    }
  } else {
    predictedCausalityStrength <- NA
    realCausalityStrength <- NA
  }
  return(data.frame(real=realCausalityStrength,
                    predicted=predictedCausalityStrength))
}
#=== Prerequisites
norm_vec <- function(x) sqrt(sum(x^2))
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1