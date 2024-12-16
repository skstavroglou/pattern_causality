#' Fill Pattern Causality Matrix
#' 
#' @title Fill Pattern Causality Matrix
#' @description Internal function that computes causality strengths by comparing 
#' predicted and real patterns/signatures in a system's dynamic model. Uses either 
#' weighted or binary normalization to quantify causal influences.
#'
#' @param weighted Logical; if TRUE, uses error function for normalization
#' @param predictedPatternY Numeric; predicted pattern of Y
#' @param realPatternY Numeric; actual pattern of Y
#' @param predictedSignatureY Numeric vector; predicted signature of Y
#' @param realSignatureY Numeric vector; actual signature of Y
#' @param patternX Numeric; current pattern of X
#' @param signatureX Numeric vector; current signature of X
#' @param verbose Logical; if TRUE, prints computation details
#'
#' @return A pc_strength object containing:
#'   \itemize{
#'     \item real: Real causality strength
#'     \item predicted: Predicted causality strength
#'   }
#'
#' @keywords internal
#' @noRd
fillPCMatrix <- function(weighted, predictedPatternY, realPatternY, 
                        predictedSignatureY, realSignatureY, 
                        patternX, signatureX, 
                        verbose = FALSE) {
  # Input validation
  if(!is.logical(weighted)) {
    stop("weighted must be TRUE or FALSE", call. = FALSE)
  }
  
  if(!is.numeric(c(predictedPatternY, realPatternY, patternX))) {
    stop("All patterns must be numeric", call. = FALSE)
  }
  
  if(!is.numeric(c(predictedSignatureY, realSignatureY, signatureX))) {
    stop("All signatures must be numeric vectors", call. = FALSE)
  }
  
  # Initialize results with NA_real_
  predictedCausalityStrength <- NA_real_
  realCausalityStrength <- NA_real_
  
  if (!anyNA(c(predictedPatternY, realPatternY, patternX))) {
    if (length(predictedPatternY) > 0 && length(patternX) > 0) {
      if (verbose) {
        cat("Computing causality strengths:\n")
        cat("Predicted pattern:", predictedPatternY, "\n")
        cat("Real pattern:", realPatternY, "\n")
      }
      
      if (predictedPatternY == realPatternY) {
        if (weighted) {
          predictedCausalityStrength <- erf(norm_vec(predictedSignatureY) / 
                                          norm_vec(signatureX))
          realCausalityStrength <- erf(norm_vec(realSignatureY) / 
                                     norm_vec(signatureX))
        } else {
          predictedCausalityStrength <- 1
          realCausalityStrength <- 1
        }
      } else {
        predictedCausalityStrength <- 0
        realCausalityStrength <- 0
      }
    } else {
      stop("Pattern vectors cannot be empty", call. = FALSE)
    }
  }
  
  # Create and return pc_strength object
  pc_strength(
    real = realCausalityStrength,
    predicted = predictedCausalityStrength
  )
}

# Helper functions
norm_vec <- function(x) sqrt(sum(x^2))
erf <- function(x) 2 * stats::pnorm(x * sqrt(2)) - 1
