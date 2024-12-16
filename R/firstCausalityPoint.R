#' Calculate First Valid Causality Point
#' 
#' @title Calculate First Valid Causality Point
#' @description Internal function that determines the earliest valid index for 
#' causality analysis in a time series, considering embedding parameters and 
#' prediction horizon. This ensures sufficient data points for reliable analysis.
#'
#' @param E Positive integer; embedding dimension
#' @param tau Positive integer; time delay
#' @param h Non-negative integer; prediction horizon
#' @param X Numeric vector; time series data
#' @param verbose Logical; if TRUE, prints computation details
#'
#' @return A pc_point object containing:
#'   \itemize{
#'     \item point: First valid index for causality analysis
#'     \item spans: List of spans used in calculation
#'   }
#'
#' @keywords internal
#' @noRd
firstCausalityPoint <- function(E, tau, h, X, verbose = FALSE) {
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
  cc_span <- (E - 1) * tau  # Remove common coordinate NNs
  pred_span <- h
  
  if(verbose) {
    cat("Computing first causality point:\n")
    cat("Nearest neighbor span:", nn_span, "\n")
    cat("Common coordinate span:", cc_span, "\n")
    cat("Prediction span:", pred_span, "\n")
  }
  
  # Calculate first causality point
  fcp <- 1 + nn_span + cc_span + pred_span
  
  # Validate sufficient data length
  if(nn_span + cc_span + pred_span >= length(X) - cc_span) {
    stop(
      sprintf(
        paste(
          "Insufficient data for causality analysis.",
          "Required length: %d, Available length: %d\n",
          "Check parameters: E=%d, tau=%d, h=%d"
        ),
        nn_span + 2 * cc_span + pred_span,
        length(X),
        E, tau, h
      ),
      call. = FALSE
    )
  }
  
  # Create and return pc_point object
  pc_point(
    point = fcp,
    spans = list(
      nn = nn_span,
      cc = cc_span,
      pred = pred_span
    )
  )
}
