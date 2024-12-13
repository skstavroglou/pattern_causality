% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\name{plot_total}
\alias{plot_total}
\title{Plot Total Pattern Causality}
\usage{
plot_total(x, ...)
}
\arguments{
\item{x}{An object containing pattern causality results, typically a \code{pc_fit} object.}

\item{...}{Additional arguments passed to the underlying plotting functions.}
}
\value{
NULL invisibly.
}
\description{
Visualizes the total pattern causality strength as a barplot.
This function takes a \code{pc_fit} object and generates a barplot showing the
overall causality strength.
}
\examples{
data(climate_indices)
X <- climate_indices$AO
Y <- climate_indices$AAO
pc_result <- pcLightweight(X, Y, E = 3, tau = 2, metric = "euclidean", h = 1, weighted = TRUE)
plot_total(pc_result)
}
\references{
Stavroglou et al. (2020) \doi{10.1073/pnas.1918269117}
}
\seealso{
\code{\link{plot_components}} for visualizing individual causality components.
}
