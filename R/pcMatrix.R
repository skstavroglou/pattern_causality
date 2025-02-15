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
#' @param relative Logical; if TRUE calculates relative changes ((new-old)/old), if FALSE calculates absolute changes (new-old) in signature space. Default is TRUE.
#' @param verbose Logical; whether to print progress
#' @param n_cores Integer; number of cores for parallel computation
#' @return A pc_matrix object containing causality matrices
#' @export
pcMatrix <- function(dataset, E, tau, metric="euclidean", h, weighted = TRUE, 
                    distance_fn = NULL, state_space_fn = NULL,
                    relative = TRUE, verbose = FALSE, n_cores = 1) {
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
  if(!is.numeric(n_cores) || n_cores < 1) {
    stop("n_cores must be a positive integer", call. = FALSE)
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
  
  # Parallel computation setup
  if (n_cores > 1) {
    if(verbose) cat("Setting up parallel computation with", n_cores, "cores...\n")
    
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))
    
    # Setup working environment
    parallel::clusterEvalQ(cl, {
      options(digits = 15)
      options(scipen = 999)
    })
    
    # Export required functions and data
    parallel::clusterExport(cl, c("dataset", "E", "tau", "metric", 
                                 "h", "weighted", "distance_fn", 
                                 "state_space_fn", "relative", "pcLightweight"), 
                           envir = environment())
    
    # Create computation grid for non-diagonal elements
    grid <- expand.grid(i = 1:n, j = 1:n)
    grid <- grid[grid$i != grid$j, ]
    
    # Parallel computation
    results <- parallel::parLapply(cl, 1:nrow(grid), function(idx) {
      i <- grid$i[idx]
      j <- grid$j[idx]
      pc <- pcLightweight(dataset[,i], dataset[,j], 
                         E, tau, metric=metric, h, weighted,
                         distance_fn=distance_fn,
                         state_space_fn=state_space_fn,
                         relative=relative,
                         verbose = FALSE)
      list(i=i, j=j, pc=pc)
    })
    
    # Fill matrices with results
    for (res in results) {
      matrices$positive[res$i, res$j] <- res$pc$positive
      matrices$negative[res$i, res$j] <- res$pc$negative
      matrices$dark[res$i, res$j] <- res$pc$dark
    }
    
  } else {
    # Sequential computation
    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          pc <- pcLightweight(dataset[,i], dataset[,j], 
                            E, tau, metric=metric, h, weighted,
                            distance_fn=distance_fn,
                            state_space_fn=state_space_fn,
                            relative=relative,
                            verbose = FALSE)
          
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
  }
  
  # Create pc_matrix object and return
  result <- pc_matrix(
    positive = matrices$positive,
    negative = matrices$negative,
    dark = matrices$dark,
    items = items,
    verbose = verbose
  )
  
  result$is_square <- TRUE
  result$parameters <- list(
    E = E,
    tau = tau,
    metric = metric,
    h = h,
    weighted = weighted,
    n_cores = n_cores
  )
  
  return(result)
}