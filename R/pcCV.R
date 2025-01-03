#' Perform Pattern Causality Cross-Validation Analysis
#' 
#' @title Pattern Causality Cross-Validation Analysis
#' @description Evaluates the robustness of pattern causality measures through 
#' repeated sampling analysis. This function performs cross-validation by analyzing 
#' multiple subsets of the data to assess the stability of causality relationships.
#'
#' @param X Numeric vector representing the first time series.
#' @param Y Numeric vector representing the second time series.
#' @param E Integer specifying the embedding dimension.
#' @param tau Integer specifying the time delay.
#' @param metric Character string specifying the distance metric to use.
#' @param h Integer specifying the prediction horizon.
#' @param weighted Logical indicating whether to use weighted calculations.
#' @param distance_fn Optional custom distance function.
#' @param state_space_fn Optional custom state space function.
#' @param numberset Numeric vector of sample sizes to analyze.
#' @param random Logical indicating whether to use random sampling (default: TRUE).
#' @param bootstrap Integer specifying the number of bootstrap iterations (default: 1).
#' @param verbose Logical indicating whether to display progress messages.
#' @param n_cores Integer specifying the number of cores to use for parallel computation (default: 1).
#'
#' @details
#' The function implements these key steps:
#' \itemize{
#'   \item Validates input parameters and data
#'   \item Performs stratified sampling of time series data
#'   \item When random=TRUE and bootstrap>1, performs bootstrap sampling
#'   \item Computes pattern causality measures for each sample
#'   \item Aggregates results across all samples
#' }
#'
#' When bootstrap sampling is enabled (random=TRUE and bootstrap>1), the function returns
#' statistics including mean, 5% quantile, 95% quantile, and median for each sample size.
#'
#' @return A pc_cv object containing:
#' \itemize{
#'   \item samples: Vector of sample sizes used
#'   \item results: Array of causality results
#'   \item parameters: List of analysis parameters
#' }
#'
#' The results array structure depends on the bootstrap parameter:
#' \itemize{
#'   \item If bootstrap>1: A three-dimensional array where first dimension represents
#'         sample sizes, second dimension contains statistics (mean, quantiles, median),
#'         and third dimension represents causality types (positive, negative, dark)
#'   \item If bootstrap=1: A three-dimensional array where first dimension represents
#'         sample sizes, second dimension contains single values, and third dimension
#'         represents causality types (positive, negative, dark)
#' }
#'
#' @examples
#' \donttest{
#' data(climate_indices)
#' X <- climate_indices$AO
#' Y <- climate_indices$AAO
#' 
#' # Basic cross-validation
#' cv_result <- pcCrossValidation(
#'   X, Y, 
#'   E = 3, tau = 1,
#'   metric = "euclidean",
#'   h = 1,
#'   weighted = FALSE,
#'   numberset = c(100, 200, 300)
#' )
#' 
#' # Cross-validation with bootstrap
#' cv_result_boot <- pcCrossValidation(
#'   X, Y,
#'   E = 3, tau = 1,
#'   metric = "euclidean",
#'   h = 1,
#'   weighted = FALSE,
#'   numberset = c(100, 200, 300),
#'   random = TRUE,
#'   bootstrap = 100
#' )
#' }
#' @seealso 
#' \code{\link{plot.pc_cv}} for visualizing cross-validation results
#' \code{\link{print.pc_cv}} for printing cross-validation results
#' \code{\link{summary.pc_cv}} for summarizing cross-validation results
#'
#' @export
pcCrossValidation <- function(X, Y, E, tau, metric = "euclidean", h, weighted,  
                             distance_fn = NULL,
                             state_space_fn = NULL,
                             numberset, random = TRUE, bootstrap = 1, 
                             verbose = FALSE,
                             n_cores = 1) {
  
  # Input validation
  if(!is.logical(random)) {
    stop("random must be logical", call. = FALSE)
  }
  if(!is.numeric(numberset) || any(numberset <= 0)) {
    stop("numberset must contain positive numeric values", call. = FALSE)
  }
  if(max(numberset) > length(X)) {
    stop("Sample sizes cannot exceed time series length", call. = FALSE)
  }
  if(!is.numeric(bootstrap) || bootstrap < 1) {
    stop("bootstrap must be a positive integer", call. = FALSE)
  }
  if(!random && bootstrap > 1) {
    warning("bootstrap is ignored when random = FALSE", call. = FALSE)
    bootstrap <- 1
  }
  if(!is.numeric(n_cores) || n_cores < 1) {
    stop("n_cores must be a positive integer", call. = FALSE)
  }
  
  # Validate core inputs
  validate_inputs(X, Y, E, tau, metric, h, weighted, distance_fn)
  
  # Initialize results array
  numbers <- sort(numberset)
  if(random && bootstrap > 1) {
    results <- array(NA_real_, 
                    dim = c(length(numbers), 4, 3),
                    dimnames = list(
                      as.character(numbers),
                      c("mean", "5%", "95%", "median"),
                      c("positive", "negative", "dark")
                    ))
  } else {
    results <- array(NA_real_,
                    dim = c(length(numbers), 1, 3),
                    dimnames = list(
                      as.character(numbers),
                      "value",
                      c("positive", "negative", "dark")
                    ))
  }
  
  if(verbose) {
    cat("Performing cross-validation analysis...\n")
  }
  
  # Setup parallel computation if needed
  if(n_cores > 1 && random && bootstrap > 1) {
    if(verbose) cat("Setting up parallel computation with", n_cores, "cores...\n")
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))
    
    # Export required objects to worker nodes
    parallel::clusterExport(cl, c("X", "Y", "E", "tau", "h", "weighted", 
                                 "metric", "distance_fn", "state_space_fn",
                                 "pcLightweight"), 
                           envir = environment())
  }
  
  # Main analysis loop
  for(i in seq_along(numbers)) {
    if(random) {
      if(bootstrap > 1) {
        # Parallel bootstrap analysis
        if(n_cores > 1) {
          # Parallel computation of bootstrap samples
          bootstrap_results <- do.call(rbind, parallel::parLapply(cl, 1:bootstrap, function(b) {
            idx <- sample(1:length(X), numbers[i], replace = TRUE)
            samplex <- X[idx]
            sampley <- Y[idx]
            
            pc_result <- pcLightweight(samplex, sampley, E, tau, h, weighted,
                                     metric = metric,
                                     distance_fn = distance_fn,
                                     state_space_fn = state_space_fn,
                                     verbose = FALSE)
            
            c(pc_result$positive, pc_result$negative, pc_result$dark)
          }))
        } else {
          # Sequential bootstrap computation
          bootstrap_results <- matrix(NA_real_, nrow = bootstrap, ncol = 3)
          for(b in 1:bootstrap) {
            idx <- sample(1:length(X), numbers[i], replace = TRUE)
            samplex <- X[idx]
            sampley <- Y[idx]
            
            pc_result <- pcLightweight(samplex, sampley, E, tau, h, weighted,
                                     metric = metric,
                                     distance_fn = distance_fn,
                                     state_space_fn = state_space_fn,
                                     verbose = FALSE)
            
            bootstrap_results[b, ] <- c(pc_result$positive,
                                      pc_result$negative,
                                      pc_result$dark)
          }
        }
        
        # Calculate statistics
        results[i, , ] <- rbind(
          colMeans(bootstrap_results),
          apply(bootstrap_results, 2, function(x) stats::quantile(x, 0.05)),
          apply(bootstrap_results, 2, function(x) stats::quantile(x, 0.95)),
          apply(bootstrap_results, 2, stats::median)
        )
      } else {
        # Single random sample
        idx <- sample(1:(length(X) - numbers[i] + 1), 1)
        samplex <- X[idx:(idx + numbers[i] - 1)]
        sampley <- Y[idx:(idx + numbers[i] - 1)]
        
        pc_result <- pcLightweight(samplex, sampley, E, tau, h, weighted,
                                 metric = metric,
                                 distance_fn = distance_fn,
                                 state_space_fn = state_space_fn,
                                 verbose = FALSE)
        
        results[i, 1, ] <- c(pc_result$positive,
                            pc_result$negative,
                            pc_result$dark)
      }
    } else {
      # Sequential sampling
      samplex <- X[1:numbers[i]]
      sampley <- Y[1:numbers[i]]
      
      pc_result <- pcLightweight(samplex, sampley, E, tau, h, weighted,
                               metric = metric,
                               distance_fn = distance_fn,
                               state_space_fn = state_space_fn,
                               verbose = FALSE)
      
      results[i, 1, ] <- c(pc_result$positive,
                          pc_result$negative,
                          pc_result$dark)
    }
    
    if(verbose) {
      report_progress(i, length(numbers), "Cross-validation analysis", verbose)
    }
  }
  
  # Return pc_cv object with modified structure
  result <- pc_cv(
    samples = numbers,
    results = results,
    parameters = list(
      E = E,
      tau = tau,
      metric = metric,
      h = h,
      weighted = weighted,
      random = random,
      bootstrap = bootstrap,
      n_cores = n_cores
    )
  )
  
  return(result)
}
