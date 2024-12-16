#' Nature of Causality Analysis
#' 
#' @title Nature of Causality Analysis
#' @description Analyzes pattern causality matrices to classify the nature of 
#' causality between variables. This function provides core functionality for 
#' pattern causality analysis and can be used both independently and as part of 
#' larger analysis workflows.
#'
#' @details This function analyzes the structure of pattern causality matrices
#' to determine four types of causality:
#' \itemize{
#'   \item No Causality: When no significant relationship is detected
#'   \item Positive Causality: When patterns show positive influence
#'   \item Negative Causality: When patterns show negative influence
#'   \item Dark Causality: When patterns show complex or indirect influence
#' }
#'
#' @param PC Three-dimensional array; pattern causality matrices
#' @param dur Numeric vector; time points to analyze
#' @param hashedpatterns Numeric vector; pattern identifiers
#' @param X Numeric vector; reference for output length
#' @param weighted Logical; if TRUE, uses weighted causality strength
#' @param verbose Logical; if TRUE, prints computation details
#'
#' @return A pc_nature object containing:
#' \itemize{
#'   \item no_causality: Vector of no causality strengths
#'   \item positive: Vector of positive causality strengths  
#'   \item negative: Vector of negative causality strengths
#'   \item dark: Vector of dark causality strengths
#' }
#' 
#' @seealso 
#' \code{\link{pcLightweight}} for basic causality analysis
#' \code{\link{pcFullDetails}} for detailed analysis
#' \code{\link{pcMatrix}} for causality matrix computation
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Generate example data
#' PC <- array(runif(27), dim = c(3,3,3))
#' dur <- 1:3
#' hashedpatterns <- 1:3
#' X <- rnorm(10)
#' 
#' # Analyze causality nature
#' result <- natureOfCausality(PC, dur, hashedpatterns, X)
#' print(result)
#' }
natureOfCausality <- function(PC, dur, hashedpatterns, X, weighted = TRUE, 
                             verbose = FALSE) {
  # Input validation
  if(!is.array(PC) || length(dim(PC)) != 3) {
    stop("PC must be a 3-dimensional array", call. = FALSE)
  }
  
  if(!is.numeric(dur) || !is.numeric(hashedpatterns) || !is.numeric(X)) {
    stop("dur, hashedpatterns, and X must be numeric vectors", call. = FALSE)
  }
  
  if(!is.logical(weighted)) {
    stop("weighted must be TRUE or FALSE", call. = FALSE)
  }
  
  # Initialize vectors with NA_real_
  results <- list(
    no_causality = rep(NA_real_, length(X)),
    positive = rep(NA_real_, length(X)),
    negative = rep(NA_real_, length(X)),
    dark = rep(NA_real_, length(X))
  )
  
  if(verbose) {
    cat("Analyzing causality nature for", length(dur), "time points\n")
  }
  
  for(i in seq_along(dur)) {
    t <- dur[i]
    cell <- which(!is.na(PC[, , t]), arr.ind = TRUE)
    
    if(length(cell) > 0 && !anyNA(PC[cell[1], cell[2], t])) {
      strength <- PC[cell[1], cell[2], t]
      mid_point <- mean(1:length(hashedpatterns))
      
      # Determine causality type
      is_diagonal <- cell[1] == cell[2]
      is_antidiagonal <- (cell[1] + cell[2]) == (length(hashedpatterns) + 1)
      is_center <- cell[1] == mid_point
      
      # Set causality values
      results <- determine_causality(results, t, strength, is_diagonal, 
                                   is_antidiagonal, is_center, weighted)
    }
    
    if(verbose) {
      report_progress(i, length(dur), "Analyzing causality patterns", verbose)
    }
  }
  
  if(verbose) {
    cat("\nCausality analysis complete\n")
  }
  
  # Create and return pc_nature object
  pc_nature(
    no_causality = results$no_causality,
    positive = results$positive,
    negative = results$negative,
    dark = results$dark
  )
}

#' @keywords internal
#' @noRd
determine_causality <- function(results, t, strength, is_diagonal, is_antidiagonal, 
                              is_center, weighted) {
  if(strength == 0) {
    results$no_causality[t] <- 1
    results$positive[t] <- 0
    results$negative[t] <- 0
    results$dark[t] <- 0
  } else {
    results$no_causality[t] <- 0
    value <- if(weighted) strength else 1
    
    if(is_diagonal && !is_center) {
      results$positive[t] <- value
      results$negative[t] <- 0
      results$dark[t] <- 0
    } else if(is_antidiagonal && !is_center) {
      results$positive[t] <- 0
      results$negative[t] <- value
      results$dark[t] <- 0
    } else {
      results$positive[t] <- 0
      results$negative[t] <- 0
      results$dark[t] <- value
    }
  }
  results
}
