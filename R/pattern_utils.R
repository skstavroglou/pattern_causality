#' Print Pattern Causality Pattern Analysis Results
#' @keywords internal
#' @noRd
print.pc_pattern <- function(x, ...) {
  cat("Pattern Causality Pattern Analysis\n")
  cat("--------------------------------\n")
  cat("Number of patterns:", nrow(x$patterns), "\n")
  cat("Pattern dimension:", ncol(x$patterns), "\n")
  cat("Hash range:", range(x$hashes), "\n")
}

#' @keywords internal
#' @noRd
summary.pc_pattern <- function(object, ...) {
  structure(
    list(
      n_patterns = nrow(object$patterns),
      dimension = ncol(object$patterns),
      hash_range = range(object$hashes),
      pattern_stats = summary(as.vector(object$patterns)),
      hash_stats = summary(object$hashes)
    ),
    class = "summary.pc_pattern"
  )
} 