#========================#
#= FOUNDATION | LAYER 0 =#
#========================#
#' @title Pattern Hashing Function
#' @description This function hashes all possible patterns generated from a dataset to facilitate analysis
#' of their distribution and frequency, supporting risk assessment in decision-making processes related
#' to the causality and dynamics of complex systems.
#'
#' @param E The embedding dimension which influences the complexity and variety of patterns generated.
#' This parameter adjusts the granularity with which the system's dynamics are analyzed and interpreted.
#'
#' @return hashedpatterns Returns a vector of hashed values representing each pattern or `NA` if the
#' pattern generation was not possible, typically due to insufficient or overly simplified input.
#'
#' @examples
#' # Assume E is set to 3, which is suitable for generating moderately complex patterns.
#' hashed_result <- patternHashing(3)
#' print(hashed_result)
#' @export
#= Appointing Unique Identifiers to Symbolic Patterns
patternHashing <- function(E) {
  a <- possiblePatterns(E)
  if (!anyNA(a)) {
    hashedpatterns <- apply(a, 1, hashing)
  } else {
    hashedpatterns <- NA
  }
  return(hashedpatterns)
}
#=== Prerequisites
possiblePatterns <- function(E) {
  if (E<=1) {
    p <- NA
  } else {
    p <- as.matrix(expand.grid(rep(list(1:3), E-1)))
  }
  return(p)
}
hashing <- function(vec) {
  hash = 0
  for (i in 1:length(vec)) {
    hash = hash + vec[i]*factorial(i+2)
  }
  return(hash)
}
