#' Print Method for Pattern Causality Accuracy Results
#' 
#' @title Print Method for Pattern Causality Accuracy Results
#' @param x A pc_accuracy object
#' @param verbose Logical; whether to display detailed information (default: FALSE)
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the input object
#' @export
#' @method print pc_accuracy
print.pc_accuracy <- function(x, verbose = FALSE, ...) {
  cat("Pattern Causality Accuracy Analysis\n")
  cat("----------------------------------\n")
  cat("Parameters:\n")
  cat("  E:", x$parameters$E, "\n")
  cat("  tau:", x$parameters$tau, "\n")
  cat("  metric:", x$parameters$metric, "\n")
  cat("  h:", x$parameters$h, "\n")
  
  cat("\nCausality measures:\n")
  cat("  Total:", sprintf("%.4f", x$total), "\n")
  cat("  Positive:", sprintf("%.4f", x$positive), "\n")
  cat("  Negative:", sprintf("%.4f", x$negative), "\n")
  cat("  Dark:", sprintf("%.4f", x$dark), "\n")
  
  if(verbose && !is.null(x$matrices)) {
    cat("\nDetailed matrices available. Use summary() for statistics.\n")
  }
  cat("\n")
  invisible(x)
}

#' Summary Method for Pattern Causality Accuracy Results
#' 
#' @title Summary Method for Pattern Causality Accuracy Results
#' @param object A pc_accuracy object
#' @param ... Additional arguments passed to summary
#' @return A summary object for pc_accuracy
#' @export
#' @method summary pc_accuracy
summary.pc_accuracy <- function(object, ...) {
  # Compute additional statistics from matrices
  stats <- lapply(object$matrices, function(m) {
    c(mean = mean(m, na.rm = TRUE),
      sd = stats::sd(m, na.rm = TRUE),
      min = min(m, na.rm = TRUE),
      max = max(m, na.rm = TRUE))
  })
  
  structure(
    list(
      parameters = object$parameters,
      statistics = stats
    ),
    class = "summary.pc_accuracy"
  )
}

#' Print Method for Pattern Causality Accuracy Summary
#' 
#' @param x A summary.pc_accuracy object
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the input object
#' @export
#' @method print summary.pc_accuracy
print.summary.pc_accuracy <- function(x, ...) {
  cat("Pattern Causality Accuracy Summary\n")
  cat("--------------------------------\n")
  cat("Parameter settings:\n")
  print(x$parameters)
  cat("\nSummary statistics:\n")
  
  for(name in names(x$statistics)) {
    cat("  ", name, ":\n", sep = "")
    print(x$statistics[[name]])
  }
  
  cat("\n")
  invisible(x)
} 