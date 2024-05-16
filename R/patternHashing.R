#========================#
#= FOUNDATION | LAYER 0 =#
#========================#
#' Pattern Hashing function
#'
#'
#' @description Appointing Unique Identifiers to Symbolic Patterns
#' @param E the embedded dimensions
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
