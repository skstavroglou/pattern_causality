#' Calculate Pattern Causality Accuracy
#' 
#' @title Calculate Pattern Causality Accuracy
#' @description Evaluates the causality prediction accuracy across multiple time series 
#' within a dataset using the PC Mk. II Light method. This function analyzes pairwise 
#' causality relationships and computes different types of causality measures.
#'
#' @param dataset A matrix or data frame where each column represents a time series
#' @param E Integer; embedding dimension for state space reconstruction (E > 1)
#' @param tau Integer; time delay for state space reconstruction (tau > 0)
#' @param metric Character; distance metric to use, one of "euclidean", "manhattan", or "maximum"
#' @param h Integer; prediction horizon, indicating forecast distance (h >= 0)
#' @param weighted Logical; whether to use weighted approach in calculating causality strengths
#' @param distance_fn Optional custom distance function for computing distances (default: NULL)
#' @param state_space_fn Optional custom function for state space reconstruction (default: NULL)
#' @param verbose Logical; whether to display progress information (default: FALSE)
#'
#' @return An object of class "pc_accuracy" containing:
#' \itemize{
#'   \item parameters: List of input parameters (E, tau, metric, h, weighted)
#'   \item total: Mean total causality across all pairs
#'   \item positive: Mean positive causality across all pairs
#'   \item negative: Mean negative causality across all pairs
#'   \item dark: Mean dark causality across all pairs
#'   \item matrices: Raw causality matrices for each type
#' }
#'
#' @examples
#' \donttest{
#' data(climate_indices)
#' data <- climate_indices[, -1]
#' results <- pcAccuracy(dataset = data, E = 3, tau = 1, 
#'                      metric = "euclidean", h = 1, 
#'                      weighted = TRUE, verbose = TRUE)
#' print(results)
#' }
#'
#' @seealso 
#' \code{\link{pcMatrix}} for analyzing individual causality matrices
#' \code{\link{pcLightweight}} for pairwise causality analysis
#'
#' @export
pcAccuracy <- function(dataset, E, tau, metric="euclidean", h, weighted, distance_fn = NULL,
                             state_space_fn = NULL,verbose = FALSE) {
  # Input validation
  if (!is.matrix(dataset) && !is.data.frame(dataset)) {
    stop("dataset must be a matrix or data frame", call. = FALSE)
  }
  
  if(!is.character(metric) || !metric %in% c("euclidean", "manhattan", "maximum")) {
    stop("metric must be one of: 'euclidean', 'manhattan', 'maximum'", call. = FALSE)
  }
  
  dataset <- as.matrix(dataset)
  n_series <- ncol(dataset)
  
  # Initialize storage matrices with NA_real_
  matrices <- list(
    total = matrix(NA_real_, n_series, n_series),
    positive = matrix(NA_real_, n_series, n_series),
    negative = matrix(NA_real_, n_series, n_series),
    dark = matrix(NA_real_, n_series, n_series)
  )
  
  if (verbose) {
    cat("Analyzing causality relationships...\n")
  }
  
  # Pre-check feasibility for all series
  feasible_series <- sapply(1:n_series, function(i) {
    check_causality_points(E, tau, h, dataset[, i], verbose = FALSE)$feasible
  })
  
  # Main analysis loop with pre-checked series
  for (i in which(feasible_series)) {
    for (j in which(feasible_series)) {
      if (i != j) {
        result <- pcLightweight(dataset[,i], dataset[,j],
                              E, tau, metric=metric, h, weighted, distance_fn,
                              state_space_fn = state_space_fn, verbose = FALSE)
        
        matrices$total[i,j] <- result$total
        matrices$positive[i,j] <- result$positive
        matrices$negative[i,j] <- result$negative
        matrices$dark[i,j] <- result$dark
        
        if (verbose) {
          counter <- (i-1)*(n_series-1) + j
          report_progress(counter, n_series * (n_series-1), "Analyzing relationships", verbose)
        }
      }
    }
  }
  
  if (verbose) {
    cat("\nComputing summary statistics...\n")
  }
  
  # Compute summary statistics
  total_mean <- mean(matrices$total, na.rm = TRUE)
  positive_mean <- mean(matrices$positive, na.rm = TRUE)
  negative_mean <- mean(matrices$negative, na.rm = TRUE)
  dark_mean <- mean(matrices$dark, na.rm = TRUE)
  
  # Create pc_accuracy object
  result <- structure(
    list(
      total = total_mean,
      positive = positive_mean,
      negative = negative_mean,
      dark = dark_mean,
      matrices = matrices,
      parameters = list(E = E, tau = tau, metric = metric, h = h, weighted = weighted)
    ),
    class = "pc_accuracy"
  )
  
  return(result)
}
