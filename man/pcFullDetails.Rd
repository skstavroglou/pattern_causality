% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PC.Mk.II.Full.Details.R
\name{pcFullDetails}
\alias{pcFullDetails}
\title{Calculate Full Details Pattern Causality Analysis}
\usage{
pcFullDetails(
  X,
  Y,
  E,
  tau,
  h,
  weighted,
  metric = "euclidean",
  distance_fn = NULL,
  state_space_fn = NULL,
  relative = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{X}{Numeric vector; the first time series data}

\item{Y}{Numeric vector; the second time series data}

\item{E}{Integer; embedding dimension for state space reconstruction}

\item{tau}{Integer; time delay between data points}

\item{h}{Integer; prediction horizon for causality analysis}

\item{weighted}{Logical; whether to weight causality strength}

\item{metric}{Character; distance metric ('euclidean', 'manhattan', or 'maximum')}

\item{distance_fn}{Optional custom distance function for computing distances (default: NULL)}

\item{state_space_fn}{Optional custom function for state space reconstruction (default: NULL)}

\item{relative}{Logical; if TRUE calculates relative changes ((new-old)/old), if FALSE calculates absolute changes (new-old) in signature space. Default is TRUE.}

\item{verbose}{Logical; if TRUE, prints computation progress (default: FALSE)}
}
\value{
A pc_full_details object containing:
\itemize{
\item backtest_time: Time points used for backtesting
\item valid_time: Valid time points for analysis
\item causality_real: Real causality spectrum
\item causality_pred: Predicted causality spectrum
\item state_spaces: State space reconstructions
\item neighbors: Nearest neighbor information
\item patterns: Pattern and signature information
\item matrices: Causality matrices
\item predictions: Predicted and actual values
\item weighted: A logical indicating if weighted calculations were used
\item E: Embedding dimension used for the analysis
}
}
\description{
Implements an advanced pattern causality algorithm to explore the
causal relationships between two time series datasets. This function provides
comprehensive analysis of causality patterns, including state space reconstruction,
pattern identification, and causality strength evaluation.
}
\details{
Calculate Full Details Pattern Causality Analysis

The function implements these key steps:
\itemize{
\item State Space Reconstruction: Creates shadow attractors using embedding
\item Pattern Analysis: Converts time series into signature and pattern spaces
\item Nearest Neighbor Analysis: Identifies and analyzes local dynamics
\item Causality Evaluation: Computes predicted and actual causality matrices
\item Results Validation: Provides detailed diagnostics and quality metrics
}
}
