#' @keywords internal
#' @noRd
pc_nature <- function(no_causality, positive, negative, dark) {
  structure(
    list(
      no_causality = no_causality,
      positive = positive,
      negative = negative,
      dark = dark
    ),
    class = "pc_nature"
  )
}

#' @export
print.pc_nature <- function(x, ...) {
  cat("Pattern Causality Nature Analysis\n")
  cat("--------------------------------\n")
  cat("No Causality:", sum(x$no_causality, na.rm = TRUE), "\n")
  cat("Positive Causality:", sum(x$positive > 0, na.rm = TRUE), "\n")
  cat("Negative Causality:", sum(x$negative > 0, na.rm = TRUE), "\n")
  cat("Dark Causality:", sum(x$dark > 0, na.rm = TRUE), "\n")
} 