#' Transform Signature Matrix into Pattern Space Matrix
#'
#' @title Transform Signature Matrix into Pattern Space Matrix
#' @description Transforms a signature matrix into a pattern space matrix by processing 
#' each row to reflect categorical changes (increase, decrease, no change) in sequence 
#' values. These changes are then hashed to create unique pattern identifiers.
#'
#' @param SM Numeric matrix; signature matrix where each row represents differences between 
#' successive elements in the original time series data.  Must contain only numeric values.
#'
#' @return A numeric matrix where each row contains hashed pattern identifiers derived 
#' from categorical changes in the signature matrix. Returns a matrix with NA values if any row contains NA values.
#'
#' @details
#' The function performs these key steps:
#' \itemize{
#'   \item Converts numerical differences into categorical changes (1: decrease, 2: no change, 3: increase)
#'   \item Applies a hashing function (detailed below) to generate unique identifiers for each pattern
#'   \item Handles missing values by returning NA for rows containing NA values.
#' }
#'
#' @examples
#' signatureMatrix <- matrix(c(1, -1, 0, 2, -2, 0, 1, -1, 0), 
#'                          nrow = 3, byrow = TRUE)
#' patternSpaceMatrix <- patternSpace(signatureMatrix)
#' print(patternSpaceMatrix)
#'
#' @keywords internal
#' @noRd
patternSpace <- function(SM) {
  # Input validation
  if (!is.matrix(SM) || !is.numeric(SM)) {
    stop("Input must be a numeric matrix", call. = FALSE)
  }
  
  # Transform to pattern space
  do.call(rbind, sapply(seq_len(nrow(SM)), function(i) patternVectorDifference(SM[i, ]), simplify = FALSE))
}

#' Convert Signature Vector to Pattern Vector
#'
#' @param sVec Numeric vector; signature vector to be converted
#' @return Numeric vector; hashed pattern identifier
#' @keywords internal
#' @noRd
patternVectorDifference <- function(sVec) {
  if (anyNA(sVec)) {
    return(rep(NA_real_, length(sVec)))
  }
  
  # Convert to categorical changes
  p.vec <- ifelse(sVec > 0, 3, 
                 ifelse(sVec < 0, 1, 2))
  
  # Generate hash (using a more efficient approach if needed)
  hashing(p.vec)
}

#' Generate Hash Value for Pattern Vector
#'
#' @param vec Numeric vector; pattern vector to be hashed
#' @return Numeric; hash value
#' @keywords internal
#' @noRd
hashing <- function(vec) {
  if (anyNA(vec)) {
    return(NA_real_)
  }
  sum(vec * factorial(seq_along(vec) + 2))
}
