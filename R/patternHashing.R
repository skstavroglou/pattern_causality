#' @keywords internal
pc_pattern <- function(patterns, hashes) {
  structure(
    list(
      patterns = patterns,
      hashes = hashes
    ),
    class = "pc_pattern"
  )
}

#' Generate Pattern Hashes for Pattern Causality Analysis
#' 
#' @title Generate Pattern Hashes for Pattern Causality Analysis
#' @description Internal function that generates and hashes patterns for the pattern
#' causality algorithm. This function is a core component of the pattern causality
#' analysis framework.
#'
#' @param E Integer; embedding dimension determining pattern complexity
#' @param verbose Logical; if TRUE, prints computation progress
#' @return A pc_pattern object containing:
#' \itemize{
#'   \item patterns: Matrix of possible patterns
#'   \item hashes: Vector of corresponding hash values
#' }
#' @keywords internal
#' @noRd
patternHashing <- function(E, verbose = FALSE) {
  # Input validation
  if(!is.numeric(E) || E <= 1 || E != round(E)) {
    stop("E must be an integer greater than 1", call. = FALSE)
  }
  
  if(verbose) {
    cat("Generating patterns for embedding dimension", E, "\n")
  }
  
  # Generate possible patterns
  patterns <- as.matrix(expand.grid(rep(list(1:3), E - 1)))
  
  if(verbose) {
    cat("Computing hash values for", nrow(patterns), "patterns\n")
  }
  
  # Compute hash values
  hashes <- apply(patterns, 1, function(vec) {
    sum(vec * factorial(seq_along(vec) + 2))
  })
  
  # Create and return pc_pattern object
  pc_pattern(patterns = patterns, hashes = hashes)
}
