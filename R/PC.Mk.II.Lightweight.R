#' Calculate Pattern Causality Using Lightweight Algorithm
#' 
#' @title Calculate Pattern Causality Using Lightweight Algorithm
#' @description Implements a computationally efficient version of the Pattern Causality 
#' Model Mk. II for analyzing causal interactions between two time series. This function 
#' uses pattern and signature spaces to assess causality through reconstructed state spaces 
#' and hashed pattern analysis.
#'
#' @param X A numeric vector representing the first time series
#' @param Y A numeric vector representing the second time series
#' @param E Integer; embedding dimension for state space reconstruction (E > 1)
#' @param tau Integer; time delay for state space reconstruction (tau > 0)
#' @param metric Character string specifying the distance metric; one of "euclidean", 
#'        "manhattan", or "maximum"
#' @param h Integer; prediction horizon for future projections (h >= 0)
#' @param weighted Logical; whether to use weighted causality strength calculations
#' @param distance_fn Custom distance function for state space reconstruction
#' @param state_space_fn Custom function for state space transformation
#' @param relative Logical; if TRUE calculates relative changes ((new-old)/old), if FALSE calculates absolute changes (new-old) in signature space. Default is TRUE.
#' @param verbose Logical; whether to display progress information (default: FALSE)
#'
#' @return An object of class "pc_fit" containing:
#'   \itemize{
#'     \item total: Total causality strength (0-1)
#'     \item positive: Proportion of positive causality (0-1)
#'     \item negative: Proportion of negative causality (0-1)
#'     \item dark: Proportion of dark causality (0-1)
#'   }
#'
#' @details
#' The function implements these key steps:
#' \itemize{
#'   \item State space reconstruction using embedding parameters
#'   \item Pattern and signature space transformation
#'   \item Nearest neighbor analysis in reconstructed spaces
#'   \item Causality strength calculation using prediction accuracy
#'   \item Classification of causality types (positive/negative/dark)
#' }
#'
#' @seealso 
#' \code{\link{pcFullDetails}} for detailed analysis
#' \code{\link{pcMatrix}} for analyzing multiple time series
#'
#' @examples
#' data(climate_indices)
#' X <- climate_indices$AO
#' Y <- climate_indices$AAO
#' result <- pcLightweight(X, Y, E = 3, tau = 1, 
#'                        metric = "euclidean", h = 2, 
#'                        weighted = TRUE, verbose = FALSE)
#' print(result)
#' summary(result)
#' plot(result)
#'
#' @export
pcLightweight <- function(X, Y, E, tau, h, weighted, 
                         metric = "euclidean",
                         distance_fn = NULL,
                         state_space_fn = NULL,
                         relative = TRUE,
                         verbose = FALSE) {
  # Input validation
  validate_inputs(X, Y, E, tau, metric, h, weighted, distance_fn)
  
  # Initialize components
  components <- initialize_components(E, tau)
  
  # Compute state and pattern spaces
  spaces <- compute_spaces(X, Y, E, tau, metric,
                         distance_fn = distance_fn,
                         state_space_fn = state_space_fn,
                         relative = relative,
                         verbose = verbose)
  
  # Check causality feasibility
  causality_check <- check_causality_points(E, tau, h, X, verbose)
  if(!causality_check$feasible) {
    stop("Insufficient data length for analysis", call. = FALSE)
  }
  
  # Initialize analysis matrices
  matrices <- initialize_matrices(X, Y, E, causality_check$FCP, verbose)
  
  # Main analysis loop
  results <- analyze_causality(spaces, matrices, components, 
                             causality_check, h, weighted, verbose)
  
  # Compute final causality measures
  measures <- compute_causality_measures(results, weighted)
  
  # Create and return pc_fit object 
  result <- structure(
    list(
      total = measures$total,
      positive = measures$positive,
      negative = measures$negative,
      dark = measures$dark
    ),
    class = "pc_fit"
  )
  
  return(result)
}
