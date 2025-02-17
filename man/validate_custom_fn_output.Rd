% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pc_full_details_utils.R
\name{validate_custom_fn_output}
\alias{validate_custom_fn_output}
\title{Validate Custom Function Output for Pattern Causality Analysis}
\usage{
validate_custom_fn_output(output, fn_name)
}
\arguments{
\item{output}{The output from a custom function to validate}

\item{fn_name}{The name of the function type being validated ("distance_fn" or "state_space_fn")}
}
\value{
Nothing. Throws an error if validation fails.
}
\description{
Validates the Output Format from Custom Distance and State Space Functions
to ensure compatibility with the package's internal processing.
}
\details{
Validate Custom Function Output
}
\examples{
# Example 1: Validating custom distance function output
custom_dist <- function(x) {
  # Create distance matrix
  dist_mat <- as.matrix(dist(x))
  # Validate output
  validate_custom_fn_output(dist_mat, "distance_fn")
  return(dist_mat)
}

# Example 2: Validating custom state space function output
custom_state_space <- function(x, E, tau) {
  # Create state space matrix
  n <- length(x) - (E-1)*tau
  state_mat <- matrix(nrow = n, ncol = E)
  for(i in 1:E) {
    state_mat[,i] <- x[1:n + (i-1)*tau]
  }
  # Create output list
  result <- list(matrix = state_mat, 
                parameters = list(E = E, tau = tau))
  # Validate output
  validate_custom_fn_output(result, "state_space_fn")
  return(result)
}

# Using the custom functions
x <- sin(seq(0, 4*pi, length.out = 100))
dist_result <- custom_dist(x)
space_result <- custom_state_space(x, E = 3, tau = 2)

}
