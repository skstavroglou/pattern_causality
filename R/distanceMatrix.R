#' Calculate Distance Matrix
#' 
#' @title Calculate Distance Matrix
#' @description Internal function that computes pairwise distances between points 
#' in a state space matrix. This function is a wrapper around stats::dist that 
#' provides additional input validation and consistent output formatting for the 
#' pattern causality analysis framework.
#'
#' @param M Numeric matrix, where each row represents a point in the reconstructed 
#' state space
#' @param metric Character string specifying the distance metric:
#'   \itemize{
#'     \item "euclidean": Euclidean distance (default)
#'     \item "maximum": Maximum distance
#'     \item "manhattan": Manhattan distance
#'   }
#' @param as_matrix Logical; if TRUE, returns a full matrix, if FALSE returns a dist object
#' @param verbose Logical; if TRUE, prints information about the computation
#'
#' @return If as_matrix is TRUE, returns a symmetric matrix of distances.
#' If FALSE, returns a dist object.
#'
#' @keywords internal
#' @noRd
distanceMatrix <- function(M, metric = "euclidean", as_matrix = TRUE, verbose = FALSE) {
  # Input validation
  if(!is.matrix(M) || !is.numeric(M)) {
    stop("M must be a numeric matrix", call. = FALSE)
  }
  
  valid_metrics <- c("euclidean", "maximum", "manhattan")
  if(!metric %in% valid_metrics) {
    stop(sprintf("metric must be one of: %s", 
                paste(valid_metrics, collapse = ", ")), 
         call. = FALSE)
  }
  
  if(verbose) {
    cat(sprintf("Computing %s distances for matrix with dimensions %d x %d\n", 
                metric, nrow(M), ncol(M)))
  }
  
  # Compute distances
  d <- stats::dist(M, method = metric, upper = TRUE)
  
  # Return result in requested format
  if(as_matrix) {
    if(verbose) cat("Converting to full matrix\n")
    return(as.matrix(d))
  } else {
    return(d)
  }
}