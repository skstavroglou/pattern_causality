#' Projected Nearest Neighbors Information
#' 
#' @title Projected Nearest Neighbors Information (Internal)
#' @description Internal function that extracts and returns information about the 
#' projected nearest neighbors in a time series context, specifically useful for 
#' understanding interactions in dynamic complex systems.
#' 
#' @param My Matrix of coordinates in the original space
#' @param Dy Distance matrix, representing distances between elements in My
#' @param SMy Matrix of signatures, capturing essential patterns
#' @param PSMy Matrix of patterns, representing characteristic configurations
#' @param timesX Index at which the projection starts
#' @param i Index of the specific element for analysis
#' @param h Horizon over which the projection is considered
#' 
#' @return A pc_neighbors object containing:
#'   \itemize{
#'     \item i: Reference point index
#'     \item times: Projected time points
#'     \item dists: Distance measures
#'     \item weights: Distance-based weights
#'     \item signatures: Signature space projections
#'     \item patterns: Pattern space projections
#'     \item coordinates: Nearest neighbor coordinates
#'   }
#'
#' @keywords internal
#' @noRd
projectedNNsInfo <- function(My, Dy, SMy, PSMy, timesX, i, h) {
  # Input validation
  if(!is.matrix(My) || !is.matrix(Dy) || !is.matrix(SMy) || !is.matrix(PSMy)) {
    stop("All inputs must be matrices", call. = FALSE)
  }
  
  # Calculate weights using distance information
  w <- weightsRelativeToDistance(Dy[i, timesX + h])
  
  # Create and return pc_neighbors object
  pc_neighbors(
    i = i,
    times = timesX + h,
    dists = Dy[i, timesX + h],
    weights = as.vector(w),
    signatures = SMy[timesX + h, ],
    patterns = PSMy[timesX + h],
    coordinates = My[timesX + h, ]
  )
}

# Helper function for weight calculation
weightsRelativeToDistance <- function(distsVec) {
  weights <- distsVec
  w_total <- sum(weights)
  if (w_total == 0) w_total <- 0.0001
  
  weights <- weights / w_total
  exp(-weights) / sum(exp(-weights))
}
