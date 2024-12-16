#' Pattern Causality Parameter Optimization Results
#' 
#' @title Pattern Causality Parameter Optimization Results
#' @description Creates an object containing parameter optimization results for pattern causality analysis
#' @param accuracy_summary Data frame containing accuracy results for different parameter combinations
#' @param computation_time Time taken for optimization
#' @param parameters List of optimization parameters
#' @return An object of class "pc_params"
#' @export
pc_params <- function(accuracy_summary, computation_time, parameters) {
  structure(
    list(
      accuracy_summary = accuracy_summary,
      computation_time = computation_time,
      parameters = parameters
    ),
    class = "pc_params"
  )
}

#' Print Method for Pattern Causality Parameter Results
#' 
#' @param x A pc_params object
#' @param verbose Logical; whether to display detailed information
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the input object
#' @export
#' @method print pc_params
print.pc_params <- function(x, verbose = FALSE, ...) {
  cat("Pattern Causality Parameter Optimization Results\n")
  cat("---------------------------------------------\n")
  cat("Computation time:", format(x$computation_time), "\n\n")
  
  if(verbose) {
    cat("Parameters tested:\n")
    cat("  Emax:", x$parameters$Emax, "\n")
    cat("  tauMax:", x$parameters$tauMax, "\n")
    cat("  Metric:", x$parameters$metric, "\n\n")
    print(x$accuracy_summary)
  } else {
    cat("Parameters tested:\n")
    cat("  Emax:", x$parameters$Emax, "\n")
    cat("  tauMax:", x$parameters$tauMax, "\n")
    cat("  Metric:", x$parameters$metric, "\n\n")
    cat("First few values:\n")
    print(utils::head(x$accuracy_summary))
    if(nrow(x$accuracy_summary) > 6) {
      cat("... [truncated]\n")
    }
  }
  cat("\n")
  invisible(x)
}

#' Summary Method for Pattern Causality Parameter Results
#' 
#' @param object A pc_params object
#' @param ... Additional arguments passed to summary
#' @return A summary object for pc_params
#' @export
#' @method summary pc_params
summary.pc_params <- function(object, ...) {
  structure(
    list(
      computation_time = object$computation_time,
      parameters = object$parameters,
      accuracy_stats = summary(object$accuracy_summary)
    ),
    class = "summary.pc_params"
  )
}

# Helper function for creating accuracy summary
create_accuracy_summary <- function(results, E_array, tau_array) {
  accuracy_df <- data.frame(
    E = rep(E_array, each = length(tau_array)),
    tau = rep(tau_array, times = length(E_array)),
    Total = as.vector(t(results$total)),
    Positive = as.vector(t(results$positive)),
    Negative = as.vector(t(results$negative)),
    Dark = as.vector(t(results$dark))
  )
  accuracy_df <- accuracy_df[order(accuracy_df$E, accuracy_df$tau),]
  rownames(accuracy_df) <- sprintf("Combo %d", seq_len(nrow(accuracy_df)))
  
  return(accuracy_df)
} 