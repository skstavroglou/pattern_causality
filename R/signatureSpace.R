#' Calculate Signature Space Matrix
#' 
#' @title Calculate Signature Space Matrix From State Space
#' @description Calculates the Signature Space Matrix From a State Space Matrix By 
#' Computing Differences Between Successive Elements in Each Row. This transformation 
#' helps capture the dynamic patterns in the time series data.
#' 
#' @param M A state space matrix where each row represents a point in state space
#' @param relative Logical, if TRUE calculates relative changes ((new-old)/old), 
#'   if FALSE calculates absolute changes (new-old). Default is TRUE.
#' @return A signature space matrix where each row contains the differences between 
#'   successive elements of the corresponding state space row
#' @details The function performs the following steps:
#'   1. Validates input matrix dimensions
#'   2. For each row, computes differences between successive elements
#'   3. Returns the transformed matrix maintaining the same number of rows
#'   
#'   Similar functionality can be found in the 'nonlinearTseries' package's 
#'   embedding functions, but this implementation is specifically tailored for 
#'   pattern causality analysis.
#' 
#' @keywords internal
#' @noRd
signatureSpace <- function(M, relative = TRUE) {
  # Input validation
  if(!is.matrix(M)) {
    stop("Input must be a matrix", call. = FALSE)
  }
  
  E <- ncol(M)
  if(E < 2) {
    stop("State space matrix must have at least 2 columns", call. = FALSE)
  }
  
  # Compute signature space
  if(E == 2) {
    SM <- matrix(apply(M, 1, signatureVectorDifference, relative = relative), ncol = 1)
  } else {
    SM <- t(apply(M, 1, signatureVectorDifference, relative = relative))
  }
  
  # Ensure numeric missing values are NA_real_
  SM[is.na(SM)] <- NA_real_
  
  return(SM)
}

#' Compute Differences Between Successive Elements
#' 
#' @param vec Numeric vector of state space coordinates
#' @param relative Logical, if TRUE calculates relative changes, if FALSE calculates absolute changes
#' @return Numeric vector of differences between successive elements
#' @keywords internal
#' @noRd
signatureVectorDifference <- function(vec, relative = TRUE) {
  if(!is.numeric(vec)) {
    stop("Input must be a numeric vector", call. = FALSE)
  }
  
  if(relative) {
    # Relative change: (new - old) / old
    diffs <- diff(vec) / vec[-length(vec)]
  } else {
    # Absolute change: new - old
    diffs <- diff(vec)
  }
  
  return(diffs)
}
