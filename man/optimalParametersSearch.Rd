% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimalParametersSearch.R
\name{optimalParametersSearch}
\alias{optimalParametersSearch}
\title{Search for Optimal Parameters in Pattern Causality Analysis}
\usage{
optimalParametersSearch(
  Emax,
  tauMax,
  metric = "euclidean",
  distance_fn = NULL,
  state_space_fn = NULL,
  dataset,
  h = 0,
  weighted = FALSE,
  relative = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{Emax}{Positive integer > 2; maximum embedding dimension to test}

\item{tauMax}{Positive integer; maximum time delay to test}

\item{metric}{Character string; distance metric for causality analysis ('euclidean', 'manhattan', 'maximum'). Defaults to "euclidean". Ignored if \code{distance_fn} is provided.}

\item{distance_fn}{Optional custom distance function; takes two numeric vectors as input and returns a numeric distance. (default: NULL)}

\item{state_space_fn}{Optional custom function for state space reconstruction; takes a numeric vector and parameters E and tau as input and returns a reconstructed state space. (default: NULL)}

\item{dataset}{Numeric matrix; each column represents a time series.}

\item{h}{Positive integer; prediction horizon.}

\item{weighted}{Logical; if TRUE, weighted causality analysis is performed.}

\item{relative}{Logical; if TRUE calculates relative changes ((new-old)/old), if FALSE calculates absolute changes (new-old) in signature space. Default is TRUE.}

\item{verbose}{Logical; if TRUE, prints progress information. (default: FALSE)}
}
\value{
A \code{pc_params} object containing:
\itemize{
\item accuracy_summary: A data frame summarizing the accuracy for each parameter combination.
\item computation_time: The time taken for the analysis.
\item parameters: A list of the input parameters used.
}
}
\description{
Searches for the optimal embedding dimension (E) and time delay (tau)
to maximize the accuracy of causality predictions in a dataset. This function
implements a grid search approach to evaluate different parameter combinations.
}
\details{
Search for Optimal Parameters in Pattern Causality Analysis

This function evaluates each combination of embedding dimension and time
delay for their effectiveness in detecting different types of causality:
\itemize{
\item Total causality: Overall causal relationship strength
\item Positive causality: Direct positive influences
\item Negative causality: Direct negative influences
\item Dark causality: Complex or indirect causal relationships
}
}
\examples{
\donttest{
data(climate_indices)
dataset <- climate_indices[, -1]
optimalParams <- optimalParametersSearch(
  Emax = 3, 
  tauMax = 3, 
  metric = "euclidean",
  dataset = dataset,
  h = 1,
  weighted = FALSE
)
print(optimalParams)
}
}
