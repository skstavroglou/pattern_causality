#' State Space Reconstruction Analysis
#' 
#' @title State Space Reconstruction
#' @description Reconstructs the state space of a time series using delay embedding,
#' creating a matrix where each row represents a point in the reconstructed space.
#'
#' @details
#' The function implements Takens' embedding theorem to reconstruct state space:
#' \itemize{
#'   \item Creates delay vectors using specified embedding dimension (E)
#'   \item Applies time delay (tau) between consecutive elements
#'   \item Handles boundary conditions and missing values
#' }
#'
#' @section Related Packages:
#' \itemize{
#'   \item \pkg{nonlinearTseries}: Nonlinear time series analysis
#'   \item \pkg{tseriesChaos}: Chaos theory analysis tools
#'   \item \pkg{fractal}: Fractal analysis methods
#' }
#'
#' @param ts Numeric vector; time series data
#' @param E Integer; embedding dimension (E > 1)
#' @param tau Integer; time delay (tau > 0)
#' @param verbose Logical; whether to display progress information
#'
#' @return An object of class "pc_state" containing:
#' \itemize{
#'   \item matrix: The reconstructed state space matrix
#'   \item parameters: List of reconstruction parameters
#'   \item original: Original time series data
#' }
#'
#' @examples
#' ts <- c(1:100)
#' result <- stateSpace(ts, E = 3, tau = 2)
#' plot(result)
#'
#' @export
stateSpace <- function(ts, E, tau, verbose = FALSE) {
  # Input validation
  if (!is.numeric(ts)) {
    stop("Time series must be numeric", call. = FALSE)
  }
  if (E < 2) {
    stop("Embedding dimension must be greater than 1", call. = FALSE)
  }
  if (tau < 1) {
    stop("Time delay must be positive", call. = FALSE)
  }
  
  if (verbose) {
    cat("Reconstructing state space...\n")
  }
  
  # Create embedding matrix
  M <- matrix(NA_real_, length(ts) - (E - 1) * tau , E)
  for (i in 1:nrow(M)) {
    M[i,] <- ts[seq(from = i, to = i + (E - 1) * tau, by = tau)]
    if (anyNA(M[i,])) {
      M[i,] <- rep(NA_real_, E)
    }
  }
  
  # Create and return pc_state object
  result <- structure(
    list(
      matrix = M,
      parameters = list(
        E = E,
        tau = tau,
        n_points = nrow(M)
      ),
      original = ts
    ),
    class = "pc_state"
  )
  
  return(result)
}
