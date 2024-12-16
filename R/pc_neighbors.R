#' Pattern Causality Neighbors Class
#'
#' @title Pattern Causality Neighbors Class
#' @description S3 class for representing nearest neighbors information in pattern 
#' causality analysis. This class is used by both past and projected neighbor analysis.
#'
#' @param i Integer; reference point index
#' @param times Numeric vector; time indices of neighbors
#' @param dists Numeric vector; distances to neighbors
#' @param signatures Matrix; signature vectors of neighbors
#' @param patterns Numeric vector; pattern indices of neighbors
#' @param coordinates Matrix; coordinate vectors of neighbors
#' @param weights Optional numeric vector; distance-based weights for neighbors
#' @return An object of class "pc_neighbors"
#' @keywords internal
#' @noRd
pc_neighbors <- function(i, times, dists, signatures, patterns, coordinates, weights = NULL) {
  # Input validation
  if(!is.numeric(i) || length(i) != 1) {
    stop("i must be a single numeric value", call. = FALSE)
  }
  if(!is.numeric(times) || !is.numeric(dists)) {
    stop("times and dists must be numeric vectors", call. = FALSE)
  }
  
  # Convert signatures to matrix if it's a vector
  if(is.null(dim(signatures))) {
    signatures <- matrix(signatures, ncol = 1)
  }
  
  # Convert coordinates to matrix if it's a vector
  if(is.null(dim(coordinates))) {
    coordinates <- matrix(coordinates, ncol = 2)  # For E=2 case
  }
  
  if(!is.matrix(signatures) || !is.matrix(coordinates)) {
    stop("signatures and coordinates must be matrices", call. = FALSE)
  }
  if(!is.numeric(patterns)) {
    stop("patterns must be a numeric vector", call. = FALSE)
  }
  if(!is.null(weights) && !is.numeric(weights)) {
    stop("weights must be NULL or a numeric vector", call. = FALSE)
  }
  
  # Create object
  structure(
    list(
      i = i,
      times = times,
      dists = dists,
      signatures = signatures,
      patterns = patterns,
      coordinates = coordinates,
      weights = weights
    ),
    class = "pc_neighbors"
  )
}

#' @export
print.pc_neighbors <- function(x, ...) {
  cat("Pattern Causality Nearest Neighbors Analysis\n")
  cat("------------------------------------------\n")
  cat("Reference point:", x$i, "\n")
  cat("Number of neighbors:", length(x$times), "\n")
  cat("Distance range:", sprintf("[%.4f, %.4f]", min(x$dists), max(x$dists)), "\n")
  if(!is.null(x$weights)) {
    cat("Weight range:", sprintf("[%.4f, %.4f]", min(x$weights), max(x$weights)), "\n")
  }
  cat("\nSignature dimension:", ncol(x$signatures), "\n")
  cat("Pattern types:", length(unique(x$patterns)), "\n")
  cat("Coordinate dimension:", ncol(x$coordinates), "\n")
}

#' @export
summary.pc_neighbors <- function(object, ...) {
  structure(
    list(
      reference_point = object$i,
      n_neighbors = length(object$times),
      distance_stats = list(
        range = range(object$dists),
        mean = mean(object$dists),
        sd = stats::sd(object$dists)
      ),
      weight_stats = if(!is.null(object$weights)) {
        list(
          range = range(object$weights),
          mean = mean(object$weights),
          sd = stats::sd(object$weights)
        )
      } else NULL,
      signature_stats = list(
        dimension = ncol(object$signatures),
        means = colMeans(object$signatures),
        sds = apply(object$signatures, 2, stats::sd)
      ),
      pattern_stats = list(
        unique = length(unique(object$patterns)),
        counts = table(object$patterns)
      )
    ),
    class = "summary.pc_neighbors"
  )
} 