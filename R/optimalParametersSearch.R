#' Search for Optimal Parameters in Pattern Causality Analysis
#' 
#' @title Search for Optimal Parameters in Pattern Causality Analysis
#' @description Searches for the optimal embedding dimension (E) and time delay (tau) 
#' to maximize the accuracy of causality predictions in a dataset. This function 
#' implements a grid search approach to evaluate different parameter combinations.
#'
#' @details This function evaluates each combination of embedding dimension and time 
#' delay for their effectiveness in detecting different types of causality:
#' \itemize{
#'   \item Total causality: Overall causal relationship strength
#'   \item Positive causality: Direct positive influences
#'   \item Negative causality: Direct negative influences
#'   \item Dark causality: Complex or indirect causal relationships
#' }
#'
#' @param Emax Positive integer > 2; maximum embedding dimension to test
#' @param tauMax Positive integer; maximum time delay to test
#' @param metric Character string; distance metric for causality analysis ('euclidean', 'manhattan', 'maximum'). Defaults to "euclidean". Ignored if `distance_fn` is provided.
#' @param dataset Numeric matrix; each column represents a time series.
#' @param h Positive integer; prediction horizon.
#' @param weighted Logical; if TRUE, weighted causality analysis is performed.
#' @param distance_fn Optional custom distance function; takes two numeric vectors as input and returns a numeric distance. (default: NULL)
#' @param state_space_fn Optional custom function for state space reconstruction; takes a numeric vector and parameters E and tau as input and returns a reconstructed state space. (default: NULL)
#' @param relative Logical; if TRUE calculates relative changes ((new-old)/old), if FALSE calculates absolute changes (new-old) in signature space. Default is TRUE.
#' @param verbose Logical; if TRUE, prints progress information. (default: FALSE)
#'
#' @return A `pc_params` object containing:
#' \itemize{
#'   \item accuracy_summary: A data frame summarizing the accuracy for each parameter combination.
#'   \item computation_time: The time taken for the analysis.
#'   \item parameters: A list of the input parameters used.
#' }
#' @export
#'
#' @examples
#' \donttest{
#' data(climate_indices)
#' dataset <- climate_indices[, -1]
#' optimalParams <- optimalParametersSearch(
#'   Emax = 3, 
#'   tauMax = 3, 
#'   metric = "euclidean",
#'   dataset = dataset,
#'   h = 1,
#'   weighted = FALSE
#' )
#' print(optimalParams)
#' }
optimalParametersSearch <- function(Emax, tauMax, metric = "euclidean", distance_fn = NULL,
                                   state_space_fn = NULL, dataset, h = 0, weighted = FALSE, 
                                   relative = TRUE, verbose = FALSE) {
  # Input validation
  if (!is.numeric(Emax) || Emax <= 2 || Emax != round(Emax)) {
    stop("Emax must be an integer greater than 2", call. = FALSE)
  }
  
  if (!is.numeric(tauMax) || tauMax <= 0 || tauMax != round(tauMax)) {
    stop("tauMax must be a positive integer", call. = FALSE)
  }
  
  # Metric validation - only if a custom distance function isn't provided
  if (is.null(distance_fn)) {
    if (!is.character(metric) || !metric %in% c("euclidean", "manhattan", "maximum")) {
      stop("metric must be one of: 'euclidean', 'manhattan', 'maximum'", call. = FALSE)
    }
  }
  
  if (!is.matrix(dataset) && !is.data.frame(dataset)) {
    stop("dataset must be a matrix or data frame", call. = FALSE)
  }
  
  dataset <- as.matrix(dataset)
  
  # Initialize results matrices
  E_array <- 2:Emax
  tau_array <- 1:tauMax
  results <- list(
    total = matrix(NA_real_, nrow = length(E_array), ncol = length(tau_array)),
    positive = matrix(NA_real_, nrow = length(E_array), ncol = length(tau_array)),
    negative = matrix(NA_real_, nrow = length(E_array), ncol = length(tau_array)),
    dark = matrix(NA_real_, nrow = length(E_array), ncol = length(tau_array))
  )
  
  start_time <- Sys.time()
  
  if (verbose) {
    message("Starting parameter optimization...")
  }
  
  # Main computation loop
  for (i in seq_along(E_array)) {
    E <- E_array[i]
    if (verbose) message(sprintf("\nTesting E: %d\n", E))
    
    for (tau in tau_array) {
      if (verbose) message(sprintf("  Testing tau: %d\n", tau))
      
      # Use pcAccuracy for evaluation
      temp <- pcAccuracy(
        dataset = dataset,
        E = E,
        tau = tau,
        metric = metric,
        h = h,
        weighted = weighted,
        distance_fn = distance_fn,
        state_space_fn = state_space_fn,
        relative = relative,
        verbose = FALSE
      )
      
      # Store results
      results$total[i, tau] <- temp$total
      results$positive[i, tau] <- temp$positive
      results$negative[i, tau] <- temp$negative
      results$dark[i, tau] <- temp$dark
    }
    
    if (verbose) {
      report_progress(i, length(E_array), "Testing parameters", verbose)
    }
  }
  
  # Process results using create_accuracy_summary helper
  accuracy_summary <- create_accuracy_summary(results, E_array, tau_array)
  
  end_time <- Sys.time()
  computation_time <- end_time - start_time
  
  if (verbose) {
    message("\nComputation completed in:", format(computation_time))
  }
  
  # Create and return pc_params object
  pc_params(
    accuracy_summary = accuracy_summary,
    computation_time = computation_time,
    parameters = list(
      Emax = Emax,
      tauMax = tauMax,
      metric = metric,
      h = h,
      weighted = weighted
    )
  )
}

#' @keywords internal
#' @noRd
zero_counter <- function(vec) {
  sum(vec == 0, na.rm = TRUE)
}

#' @keywords internal
#' @noRd
zero_filtering <- function(vec, threshold) {
  zero_counter(vec) < threshold
}

#' @keywords internal
#' @noRd
na_counter <- function(vec) {
  sum(is.na(vec))
}

#' @keywords internal
#' @noRd
na_filtering <- function(vec, threshold) {
  na_counter(vec) < threshold
}

#Helper functions (ensure these are defined elsewhere and accessible)
create_accuracy_summary <- function(results, E_array, tau_array) {
  #Implementation to create the accuracy summary data frame
  #This needs to be implemented based on your specific needs.
  #Example:
  #data.frame(E = rep(E_array, each = length(tau_array)), tau = rep(tau_array, length(E_array)), total = as.vector(results$total), positive = as.vector(results$positive), negative = as.vector(results$negative), dark = as.vector(results$dark))
}

report_progress <- function(current, total, message, verbose) {
  if (verbose) {
    message(sprintf("%s: %d/%d", message, current, total))
  }
}
