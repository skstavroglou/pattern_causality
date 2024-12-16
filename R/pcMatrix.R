#' Compute Pattern Causality Matrix Analysis
#' 
#' @title Pattern Causality Matrix Analysis
#' @description Analyzes pattern causality relationships between multiple time series 
#' by computing pairwise causality measures and organizing them into matrices.
#'
#' @details
#' The function performs these key steps:
#' \itemize{
#'   \item Validates input data and parameters
#'   \item Computes pairwise causality measures
#'   \item Organizes results into causality matrices
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
#' @param dataset Matrix or data frame of time series
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
pcMatrix <- function(dataset, E, tau, metric="euclidean", h, weighted = TRUE, distance_fn = NULL,
                             state_space_fn = NULL,verbose = FALSE) {
  if(verbose) {
    cat("Computing pattern causality matrices...\n")
  }
  # Input validation
  if (!is.matrix(dataset) && !is.data.frame(dataset)) {
    stop("dataset must be a matrix or data frame", call. = FALSE)
  }
  
  if(!is.character(metric) || !metric %in% c("euclidean", "manhattan", "maximum")) {
    stop("metric must be one of: 'euclidean', 'manhattan', 'maximum'", call. = FALSE)
  }
  
  dataset <- as.matrix(dataset)
  
  # Get item names
  items <- colnames(dataset)
  if (is.null(items)) {
    items <- paste0("V", seq_len(ncol(dataset)))
  }
  
  # Initialize matrices
  n <- ncol(dataset)
  matrices <- list(
    positive = matrix(NA_real_, nrow = n, ncol = n),
    negative = matrix(NA_real_, nrow = n, ncol = n),
    dark = matrix(NA_real_, nrow = n, ncol = n)
  )
  
  
  # Main computation loop
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        pc <- pcLightweight(dataset[,i], dataset[,j], 
                          E, tau, metric=metric, h, weighted, distance_fn=distance_fn,
                          state_space_fn=state_space_fn, verbose = FALSE)
        
        matrices$positive[i,j] <- pc$positive
        matrices$negative[i,j] <- pc$negative
        matrices$dark[i,j] <- pc$dark
        
        if (verbose) {
          counter <- (i-1)*(n-1) + j
          report_progress(counter, n * (n-1), "Computing matrices", verbose)
        }
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
    items = items,
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