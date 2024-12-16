pc_point <- function(point, spans) {
  structure(
    list(
      point = point,
      spans = spans
    ),
    class = "pc_point"
  )
}

#' @export
print.pc_point <- function(x, ...) {
  cat("First causality point:", x$point, "\n")
  cat("Spans used:\n")
  cat("  Nearest neighbors:", x$spans$nn, "\n")
  cat("  Common coordinates:", x$spans$cc, "\n")
  cat("  Prediction:", x$spans$pred, "\n")
} 