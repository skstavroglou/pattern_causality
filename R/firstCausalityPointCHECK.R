#' Check Feasibility of Causality Analysis
#' 
#' @title Check Feasibility of Causality Analysis
#' @description Internal function that validates whether a time series has 
#' sufficient length for pattern causality analysis given the specified parameters.
#'
#' @param E Positive integer; embedding dimension
#' @param tau Positive integer; time delay
#' @param h Non-negative integer; prediction horizon
#' @param X Numeric vector; time series data
#' @param verbose Logical; if TRUE, prints validation details
#'
#' @return A pc_check object containing:
#'   \itemize{
#'     \item feasible: Logical indicating if analysis is possible
#'     \item required_length: Minimum required length
#'     \item available_length: Actual length of data
#'     \item parameters: List of input parameters
#'   }
#'
#' @keywords internal
#' @noRd
firstCausalityPointCHECK <- function(E, tau, h, X, verbose = FALSE) {
  # Input validation
  if(!is.numeric(E) || E <= 0 || E != round(E)) {
    stop("E must be a positive integer", call. = FALSE)
  }
  
  if(!is.numeric(tau) || tau <= 0 || tau != round(tau)) {
    stop("tau must be a positive integer", call. = FALSE)
  }
  
  if(!is.numeric(h) || h < 0 || h != round(h)) {
    stop("h must be a non-negative integer", call. = FALSE)
  }
  
  if(!is.numeric(X)) {
    stop("X must be a numeric vector", call. = FALSE)
  }
  
  # Calculate spans
  nn_span <- E + 1  # Minimum number of nearest neighbors
  cc_span <- E - 1  # Common coordinate span
  pred_span <- h    # Prediction horizon
  
  # Calculate required length
  required_length <- nn_span + 2 * cc_span + pred_span
  available_length <- length(X)
  
  if(verbose) {
    cat("Checking causality analysis feasibility:\n")
    cat("Required length:", required_length, "\n")
    cat("Available length:", available_length, "\n")
  }
  
  # Create and return pc_check object
  pc_check(
    feasible = (nn_span + cc_span + pred_span < available_length - cc_span),
    required_length = required_length,
    available_length = available_length,
    parameters = list(
      E = E,
      tau = tau,
      h = h
    )
  )
}
