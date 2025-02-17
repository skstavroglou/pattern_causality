% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcEffect.R
\name{pcEffect}
\alias{pcEffect}
\title{Pattern Causality Effect Analysis}
\usage{
pcEffect(pcmatrix, verbose = FALSE)
}
\arguments{
\item{pcmatrix}{An object of class "pc_matrix" containing causality matrices}

\item{verbose}{Logical; whether to display computation progress (default: FALSE)}
}
\value{
An object of class "pc_effect" containing:
\itemize{
\item positive: Data frame of positive causality effects
\item negative: Data frame of negative causality effects
\item dark: Data frame of dark causality effects
\item items: Vector of component names
\item summary: Summary statistics for each causality type
}
}
\description{
Analyzes pattern causality matrices to compute and summarize the
directional effects of different causality types (positive, negative, dark)
between system components.
}
\details{
Calculate Pattern Causality Effect Analysis

The function performs these key steps:
\itemize{
\item Processes raw causality matrices
\item Computes received and exerted influence for each component
\item Calculates net causality effect (difference between received and exerted)
\item Normalizes results to percentage scale
}
}
\section{Related Packages}{

\itemize{
\item \pkg{vars}: Vector autoregression for multivariate time series
\item \pkg{lmtest}: Testing linear regression models
\item \pkg{causality}: Causality testing and modeling
}
}

\examples{
\donttest{
data(climate_indices)
dataset <- climate_indices[, -1]
pcmatrix <- pcMatrix(dataset, E = 3, tau = 1, 
                    metric = "euclidean", h = 1, 
                    weighted = TRUE)
effects <- pcEffect(pcmatrix)
print(effects)
plot(effects)
}

}
\seealso{
\code{\link{pcMatrix}} for generating causality matrices
\code{\link{plot.pc_effect}} for visualizing causality effects
}
