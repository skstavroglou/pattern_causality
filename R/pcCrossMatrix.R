#' Compute Cross Pattern Causality Matrix Analysis
#'
#' @title Cross Pattern Causality Matrix Analysis
#' @description Analyzes pattern causality relationships between multiple time series in X and multiple time series in Y
#' by computing pairwise causality measures and organizing them into a matrix.
#'
#' @details
#' The function performs these key steps:
#' \itemize{
#'   \item Validates input data and parameters
#'   \item Computes pairwise causality measures between X and Y
#'   \item Organizes results into a causality matrix
#'   \item Provides summary statistics for each causality type
#' }
#'
#' @section Related Packages:
#' \itemize{
#'   \item \pkg{vars}: Vector autoregression analysis
#'   \item \pkg{tseries}: Time series analysis tools
#'   \item \pkg{forecast}: Time series forecasting methods
#' }
#'
#' @param X Matrix or data frame of time series for the cause
#' @param Y Matrix or data frame of time series for the effect
#' @param E Integer; embedding dimension
#' @param tau Integer; time delay
#' @param metric Character; distance metric ("euclidean", "manhattan", "maximum")
#' @param h Integer; prediction horizon
#' @param weighted Logical; whether to use weighted causality
#' @param distance_fn Optional custom distance function
#' @param state_space_fn Optional custom state space reconstruction function
#' @param verbose Logical; whether to print progress
#' @return A pc_matrix object containing causality matrices
#' @export
pcCrossMatrix <- function(X, Y, E, tau, metric="euclidean", h, weighted = TRUE, distance_fn = NULL,
                         state_space_fn = NULL,verbose = FALSE) {
  if(verbose) {
    cat("Computing cross pattern causality matrix...\n")
  }
  # Input validation
  if (!is.matrix(X) && !is.data.frame(X)) {
    stop("X must be a matrix or data frame", call. = FALSE)
  }
  if (!is.matrix(Y) && !is.data.frame(Y)) {
    stop("Y must be a matrix or data frame", call. = FALSE)
  }
  
  if(!is.character(metric) || !metric %in% c("euclidean", "manhattan", "maximum")) {
    stop("metric must be one of: 'euclidean', 'manhattan', 'maximum'", call. = FALSE)
  }
  
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  
  # Get item names
  items_X <- colnames(X)
  if (is.null(items_X)) {
    items_X <- paste0("X", seq_len(ncol(X)))
  }
  items_Y <- colnames(Y)
  if (is.null(items_Y)) {
    items_Y <- paste0("Y", seq_len(ncol(Y)))
  }
  
  # Initialize matrices
  n_X <- ncol(X)
  n_Y <- ncol(Y)
  matrices <- list(
    positive = matrix(NA_real_, nrow = n_X, ncol = n_Y),
    negative = matrix(NA_real_, nrow = n_X, ncol = n_Y),
    dark = matrix(NA_real_, nrow = n_X, ncol = n_Y)
  )
  
  if (verbose) {
    cat("Computing cross pattern causality matrices...\n")
  }
  
  # Main computation loop
  for (i in 1:n_X) {
    for (j in 1:n_Y) {
      pc <- pcLightweight(X[,i], Y[,j],
                        E, tau, metric=metric, h, weighted, distance_fn=distance_fn,
                        state_space_fn=state_space_fn, verbose = FALSE)
      
      matrices$positive[i,j] <- pc$positive
      matrices$negative[i,j] <- pc$negative
      matrices$dark[i,j] <- pc$dark
      
      if (verbose) {
        counter <- (i-1)*n_Y + j
        report_progress(counter, n_X * n_Y, "Computing matrices", verbose)
      }
    }
  }
  
  if (verbose) {
    cat("\nCreating pc_matrix object...\n")
  }
  
  # Use pc_matrix constructor
  result <- pc_matrix(
    positive = matrices$positive,
    negative = matrices$negative,
    dark = matrices$dark,
    items = list(X = items_X, Y = items_Y),
    verbose = verbose
  )
  
  # Check if the matrix is square
  if (nrow(matrices$positive) != ncol(matrices$positive)) {
    result$is_square <- FALSE
  } else {
    result$is_square <- TRUE
  }
  
  # Add additional parameters
  result$parameters <- list(
    E = E,
    tau = tau,
    metric = metric,
    h = h,
    weighted = weighted
  )
  
  return(result)
} 