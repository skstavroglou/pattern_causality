#' @keywords internal
#' @noRd
pc_check <- function(feasible, required_length, available_length, parameters) {
  structure(
    list(
      feasible = feasible,
      required_length = required_length,
      available_length = available_length,
      parameters = parameters
    ),
    class = "pc_check"
  )
}

#' @export
print.pc_check <- function(x, ...) {
  cat("Causality Analysis Feasibility Check\n")
  cat("-----------------------------------\n")
  cat("Feasible:", if(x$feasible) "Yes" else "No", "\n")
  cat("Required length:", x$required_length, "\n")
  cat("Available length:", x$available_length, "\n")
  cat("\nParameters used:\n")
  cat("  E:", x$parameters$E, "\n")
  cat("  tau:", x$parameters$tau, "\n")
  cat("  h:", x$parameters$h, "\n")
  cat("\n")
} 