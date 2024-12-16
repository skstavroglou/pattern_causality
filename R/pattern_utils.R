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
summary.pc_pattern <- function(x, ...) {
  structure(
    list(
      n_patterns = nrow(x$patterns),
      dimension = ncol(x$patterns),
      hash_range = range(x$hashes),
      pattern_stats = summary(as.vector(x$patterns)),
      hash_stats = summary(x$hashes)
    ),
    class = "summary.pc_pattern"
  )
} 