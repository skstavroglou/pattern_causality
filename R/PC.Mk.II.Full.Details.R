#' Calculate Full Details Pattern Causality Analysis
#' 
#' @title Calculate Full Details Pattern Causality Analysis
#' @description Implements an advanced pattern causality algorithm to explore the 
#' causal relationships between two time series datasets. This function provides 
#' comprehensive analysis of causality patterns, including state space reconstruction,
#' pattern identification, and causality strength evaluation.
#'
#' @details The function implements these key steps:
#' \itemize{
#'   \item State Space Reconstruction: Creates shadow attractors using embedding
#'   \item Pattern Analysis: Converts time series into signature and pattern spaces
#'   \item Nearest Neighbor Analysis: Identifies and analyzes local dynamics
#'   \item Causality Evaluation: Computes predicted and actual causality matrices
#'   \item Results Validation: Provides detailed diagnostics and quality metrics
#' }
#'
#' @param X Numeric vector; the first time series data
#' @param Y Numeric vector; the second time series data
#' @param E Integer; embedding dimension for state space reconstruction
#' @param tau Integer; time delay between data points
#' @param metric Character; distance metric ('euclidean', 'manhattan', or 'maximum')
#' @param h Integer; prediction horizon for causality analysis
#' @param weighted Logical; whether to weight causality strength
#' @param distance_fn Optional custom distance function for computing distances (default: NULL)
#' @param state_space_fn Optional custom function for state space reconstruction (default: NULL)
#' @param verbose Logical; if TRUE, prints computation progress (default: FALSE)
#'
#' @return A pc_full_details object containing:
#' \itemize{
#'   \item backtest_time: Time points used for backtesting
#'   \item valid_time: Valid time points for analysis
#'   \item causality_real: Real causality spectrum
#'   \item causality_pred: Predicted causality spectrum
#'   \item state_spaces: State space reconstructions
#'   \item neighbors: Nearest neighbor information
#'   \item patterns: Pattern and signature information
#'   \item matrices: Causality matrices
#'   \item predictions: Predicted and actual values
#'   \item weighted: A logical indicating if weighted calculations were used
#'   \item E: Embedding dimension used for the analysis
#' }
#'
#' @export
pcFullDetails <- function(X, Y, E, tau, h, weighted,
                         metric = "euclidean",
                         distance_fn = NULL,
                         state_space_fn = NULL,
                         verbose = FALSE) {
  # Input validation
  validate_inputs(X, Y, E, tau, metric, h, weighted, distance_fn)
  
  # Initialize components
  components <- initialize_components(E, tau)
  hashedpatterns <- components$hashedpatterns
  
  # Compute state and pattern spaces
  spaces <- compute_spaces(X, Y, E, tau, metric,
                         distance_fn = distance_fn,
                         state_space_fn = state_space_fn,
                         verbose = verbose)
  
  # Check causality points
  causality_check <- check_causality_points(E, tau, h, X, verbose)
  if(!causality_check$feasible) {
    stop("Insufficient data length for analysis", call. = FALSE)
  }
  
  # Initialize matrices and data structures
  matrices <- initialize_matrices(X, Y, E, causality_check$FCP, verbose)
  
  # Main analysis loop
  real_loop <- numeric(0)
  for(i in causality_check$al_loop_dur) {
    if(!anyNA(c(spaces$Mx[i,], spaces$My[i + h,]))) {
      NNx <- pastNNsInfo(
        CCSPAN = (E - 1) * tau,
        NNSPAN = E + 1,
        Mx = spaces$Mx,
        Dx = spaces$Dx,
        SMx = spaces$SMx,
        PSMx = spaces$PSMx,
        i = i,
        h = h
      )
      
      if(!anyNA(NNx$dists) && !anyNA(spaces$Dy[i, NNx$times + h])) {
        real_loop <- c(real_loop, i)
        
        projNNy <- projectedNNsInfo(
          My = spaces$My,
          Dy = spaces$Dy,
          SMy = spaces$SMy,
          PSMy = spaces$PSMy,
          timesX = NNx$times,
          i = i,
          h = h
        )
        
        matrices <- update_matrices(
          matrices, spaces, NNx, projNNy, 
          i, h, weighted, verbose, hashedpatterns
        )
      }
    }
    report_progress(
      which(causality_check$al_loop_dur == i),
      length(causality_check$al_loop_dur),
      "Analyzing patterns",
      verbose
    )
  }
  
  if(verbose) {
    cat("\nComputing final results...\n")
  }
  
  spectrums <- compute_causality_spectrums(matrices$pc_matrices, real_loop, hashedpatterns, X)
  
  result <- structure(
    list(
      backtest_time = causality_check$al_loop_dur,
      valid_time = real_loop,
      causality_real = spectrums$real,
      causality_pred = spectrums$predicted,
      state_spaces = spaces[c("Mx", "My")],
      neighbors = list(NNsX = NNx),
      patterns = matrices$patterns,
      matrices = matrices$pc_matrices,
      predictions = list(
        predicted = matrices$signatures$predicted,
        real = matrices$signatures$real
      ),
      weighted = weighted,
      E=E
    ),
    class = "pc_full_details"
  )
  
  return(result)
}

