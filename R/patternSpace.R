#' Create Pattern Space from Signature Matrix
#'
#' This function transforms a signature matrix into a pattern space matrix. Each row in the signature matrix is processed to reflect categorical changes (increase, decrease, no change) in the sequence values, which are then hashed to create unique pattern identifiers for further analysis. This transformation is crucial for identifying and categorizing patterns in complex systems, facilitating the exploration of underlying causal structures.
#'
#' @param SM Matrix, the signature matrix where each row represents the differences between successive elements in the original time series data.
#' @param E Integer, the number of dimensions in the signature matrix which influences the output size of the pattern space matrix.
#'
#' @return Matrix, where each row contains hashed pattern identifiers derived from the categorical changes in the signature matrix, facilitating pattern recognition and analysis in complex systems.
#'
#' @export
#' @examples
#' signatureMatrix <- matrix(c(1, -1, 0, 2, -2, 0, 1, -1, 0), nrow = 3, byrow = TRUE)
#' patternSpaceMatrix <- patternSpace(signatureMatrix, 3)
#' print(patternSpaceMatrix)
patternSpace <- function(SM, E) {
  PSM <- as.matrix(apply(SM, 1, patternVectorDifference, E))
  return(PSM)
}

#' Pattern Vector
#' @description
#' Help funtions in the pattern difference.
#' @noRd
# @keywords internal
# @export
patternVectorDifference <- function(sVec, E) {
  if (anyNA(sVec)) {
    p.vec <- rep(NA, E - 1)
  } else {
    p.vec <- ifelse(sVec > 0, 3, ifelse(sVec < 0, 1, 2))
  }
  return(hashing(p.vec))
}

#' Hashing value generated
#' @description
#' Help function in the hashing value generation.
#' @noRd
# @keywords internal
# @export
hashing <- function(vec) {
  hash <- 0
  for (i in 1:length(vec)) {
    hash <- hash + vec[i] * factorial(i + 2)
  }
  return(hash)
}
