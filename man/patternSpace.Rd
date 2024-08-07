% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patternSpace.R
\name{patternSpace}
\alias{patternSpace}
\title{Create Pattern Space from Signature Matrix}
\usage{
patternSpace(SM, E)
}
\arguments{
\item{SM}{Matrix, the signature matrix where each row represents the differences between successive elements in the original time series data.}

\item{E}{Integer, the number of dimensions in the signature matrix which influences the output size of the pattern space matrix.}
}
\value{
Matrix, where each row contains hashed pattern identifiers derived from the categorical changes in the signature matrix, facilitating pattern recognition and analysis in complex systems.
}
\description{
This function transforms a signature matrix into a pattern space matrix. Each row in the signature matrix is processed to reflect categorical changes (increase, decrease, no change) in the sequence values, which are then hashed to create unique pattern identifiers for further analysis. This transformation is crucial for identifying and categorizing patterns in complex systems, facilitating the exploration of underlying causal structures.
}
\examples{
signatureMatrix <- matrix(c(1, -1, 0, 2, -2, 0, 1, -1, 0), nrow = 3, byrow = TRUE)
patternSpaceMatrix <- patternSpace(signatureMatrix, 3)
print(patternSpaceMatrix)
}
