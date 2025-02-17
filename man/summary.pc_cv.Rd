% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\name{summary.pc_cv}
\alias{summary.pc_cv}
\title{Summary of Pattern Causality Cross Validation Results}
\usage{
\method{summary}{pc_cv}(object, ...)
}
\arguments{
\item{object}{A \code{pc_cv} object.}

\item{...}{Additional arguments passed to the \code{summary} function.}
}
\value{
Invisibly returns the input object.
}
\description{
Provides a summary of the pattern causality cross-validation results.
This function calculates and displays summary statistics for the cross-validation
results, including sample statistics, causality statistics, and convergence.
}
\examples{
data(climate_indices)
X <- climate_indices$AO
Y <- climate_indices$AAO
numberset <- c(100, 150, 200)
cv_results <- pcCrossValidation(X, Y, 3, 2, "euclidean", 1, FALSE, numberset = numberset)
summary(cv_results)
}
