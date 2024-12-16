#' Find Past Nearest Neighbors in Pattern Causality Analysis
#' 
#' @title Find Past Nearest Neighbors in Pattern Causality Analysis
#' @description Identifies and analyzes nearest neighbors of a given point in a 
#' time series, considering temporal constraints and pattern information. This 
#' function is crucial for understanding local dynamics in the state space.
#'
#' @details The function implements these steps:
#' \itemize{
#'   \item Excludes common coordinate vectors using CCSPAN
#'   \item Excludes future points using horizon h
#'   \item Finds NNSPAN nearest neighbors based on distances
#'   \item Extracts corresponding signatures, patterns, and coordinates
#' }
#'
#' @param CCSPAN Integer; span of common coordinates to exclude
#' @param NNSPAN Integer; number of nearest neighbors to find
#' @param Mx Matrix; state space representation
#' @param Dx Matrix; distance matrix
#' @param SMx Matrix; signature matrix
#' @param PSMx Matrix; pattern matrix
#' @param i Integer; current time index
#' @param h Integer; prediction horizon
#' @param verbose Logical; if TRUE, prints computation details
#'
#' @return A pc_neighbors object containing:
#' \itemize{
#'   \item i: Current time index
#'   \item times: Indices of nearest neighbors
#'   \item dists: Distances to nearest neighbors
#'   \item signatures: Signatures of neighbors
#'   \item patterns: Patterns of neighbors
#'   \item coordinates: Coordinates of neighbors
#' }
#' @keywords internal
#' @noRd
pastNNsInfo <- function(CCSPAN, NNSPAN, Mx, Dx, SMx, PSMx, i, h, verbose = FALSE) {
  # Input validation
  if(!is.numeric(CCSPAN) || CCSPAN < 0 || CCSPAN != round(CCSPAN)) {
    stop("CCSPAN must be a non-negative integer", call. = FALSE)
  }
  
  if(!is.numeric(NNSPAN) || NNSPAN <= 0 || NNSPAN != round(NNSPAN)) {
    stop("NNSPAN must be a positive integer", call. = FALSE)
  }
  
  if(!is.matrix(Mx) || !is.matrix(Dx) || !is.matrix(SMx)) {
    stop("Mx, Dx, and SMx must be matrices", call. = FALSE)
  }
  
  if(i <= CCSPAN + h) {
    stop("Insufficient past data for the given parameters", call. = FALSE)
  }
  
  if(verbose) {
    cat("Finding nearest neighbors for point", i, "\n")
  }
  
  # Find candidate nearest neighbors
  candidateNNs <- Dx[i, 1:(i - CCSPAN - h)]
  
  # Sort and select nearest neighbors
  ordered_indices <- order(candidateNNs)[1:min(NNSPAN, length(candidateNNs))]
  times <- as.numeric(names(candidateNNs[ordered_indices]))
  dists <- candidateNNs[ordered_indices]
  
  if(verbose) {
    cat("Found", length(times), "nearest neighbors\n")
  }
  
  # Create and return pc_neighbors object
  pc_neighbors(
    i = i,
    times = times,
    dists = dists,
    signatures = SMx[times, ],
    patterns = PSMx[times],
    coordinates = Mx[times, ]
  )
}
