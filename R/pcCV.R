#' Perform Pattern Causality Cross-Validation Analysis
#' 
#' @title Pattern Causality Cross-Validation Analysis
#' @description Evaluates the robustness of pattern causality measures through 
#' repeated sampling analysis. This function performs cross-validation by analyzing 
#' multiple subsets of the data to assess the stability of causality relationships.
#'
#' @details
#' The function implements these key steps:
#' \itemize{
#'   \item Validates input parameters and data
#'   \item Performs stratified sampling of time series data
#'   \item Computes pattern causality measures for each sample
#'   \item Aggregates results across all samples
#' }
#'
#' @section Related Packages:
#' \itemize{
#'   \item \pkg{nonlinearTseries}: Provides nonlinear time series analysis tools
#'   \item \pkg{tseriesChaos}: Offers chaos theory analysis methods
#'   \item \pkg{rEDM}: Implements empirical dynamic modeling techniques
#' }
#'
#' @param X Numeric vector; first time series
#' @param Y Numeric vector; second time series
#' @param E Integer; embedding dimension for state space reconstruction (E > 1)
#' @param tau Integer; time delay for state space reconstruction (tau > 0)
#' @param metric Character; distance metric, one of "euclidean", "manhattan", "maximum"
#' @param h Integer; prediction horizon (h > 0)
#' @param weighted Logical; whether to use weighted causality calculation
#' @param distance_fn Optional custom distance function for computing distances
#' @param state_space_fn Optional custom function for state space reconstruction
#' @param numberset Numeric vector; sample sizes for cross-validation
#' @param random Logical; if TRUE, randomly sample data points; if FALSE, take sequential samples from start (default: TRUE)
#' @param verbose Logical; whether to display progress information (default: FALSE)
#'
#' @return An object of class "pc_cv" containing:
#' \itemize{
#'   \item samples: Vector of sample sizes used
#'   \item results: Matrix of causality measures for each sample
#'   \item summary: Summary statistics across all samples
#'   \item parameters: List of input parameters
#' }
#'
#' @examples
#' \donttest{
#' data(climate_indices)
#' X <- climate_indices$AO
#' Y <- climate_indices$AAO
#' numberset <- c(100, 200, 300)
#' result <- pcCrossValidation(X, Y, E = 3, tau = 1, 
#'                            metric = "euclidean", h = 2,
#'                            weighted = TRUE, numberset = numberset)
#' print(result)
#' plot(result)
#' }
#'
#' @seealso 
#' \code{\link{pcLightweight}} for basic causality analysis
#' \code{\link{pcFullDetails}} for detailed analysis
#'
#' @export
pcCrossValidation <- function(X, Y, E, tau, metric = "euclidean", h, weighted,  
                             distance_fn = NULL,
                             state_space_fn = NULL,
                             numberset, random = TRUE, verbose = FALSE) {
  
  # Additional validation for numberset and random
  if(!is.logical(random)) {
    stop("random must be logical", call. = FALSE)
  }
  if(!is.numeric(numberset) || any(numberset <= 0)) {
    stop("numberset must contain positive numeric values", call. = FALSE)
  }
  if(max(numberset) > length(X)) {
    stop("Sample sizes cannot exceed time series length", call. = FALSE)
  }
  
  # Validate core inputs
  validate_inputs(X, Y, E, tau, metric, h, weighted, distance_fn)
  
  # Initialize results matrix
  numbers <- sort(numberset)
  results <- matrix(NA_real_, nrow = length(numbers), ncol = 4)
  colnames(results) <- c("total", "positive", "negative", "dark")
  
  if(verbose) {
    cat("Performing cross-validation analysis...\n")
  }
  
  # Main analysis loop
  for(i in seq_along(numbers)) {
    if(random) {
      idx <- sample(1:(length(X) - numbers[i] + 1), 1)
      samplex <- X[idx:(idx + numbers[i] - 1)]
      sampley <- Y[idx:(idx + numbers[i] - 1)]
    } else {
      samplex <- X[1:numbers[i]]
      sampley <- Y[1:numbers[i]]
    }
    
    pc_result <- pcLightweight(samplex, sampley, E, tau, h, weighted,
                             metric = metric,
                             distance_fn = distance_fn,
                             state_space_fn = state_space_fn,
                             verbose = FALSE)
    
    results[i, ] <- c(pc_result$total,
                     pc_result$positive,
                     pc_result$negative,
                     pc_result$dark)
    
    if(verbose) {
      report_progress(i, length(numbers), "Cross-validation analysis", verbose)
    }
  }
  
  if(verbose) {
    cat("\nComputing summary statistics...\n")
  }
  
  # Create summary statistics
  summary_stats <- apply(results, 2, function(x) {
    c(mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE))
  })
  
  # Return pc_cv object
  result <- pc_cv(
    samples = numbers,
    results = results,
    summary = summary_stats,
    parameters = list(
      E = E,
      tau = tau,
      metric = metric,
      h = h,
      weighted = weighted,
      random = random
    )
  )
  
  return(result)
}
