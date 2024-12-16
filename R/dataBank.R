#' Initialize Data Storage Structure
#' 
#' @description
#' Internal function that creates and initializes various data structures for storing 
#' and managing data within the pattern causality analysis framework.
#'
#' @param type Character string specifying the data structure type:
#'   \itemize{
#'     \item "array" - Multi-dimensional array
#'     \item "vector" - One-dimensional vector
#'     \item "matrix" - Two-dimensional matrix
#'     \item "neighborhood memories" - Specialized data frame for neighborhood analysis
#'   }
#' @param dimensions Integer vector specifying dimensions
#' @param verbose Logical; if TRUE, prints information about the created structure
#'
#' @return An initialized data structure filled with NA_real_
#'
#' @keywords internal
#' @noRd
dataBank <- function(type, dimensions, verbose = FALSE) {
  # Input validation
  if(!is.character(type) || length(type) != 1) {
    stop("'type' must be a single character string", call. = FALSE)
  }
  
  if(!is.numeric(dimensions) || any(dimensions <= 0) || any(dimensions != round(dimensions))) {
    stop("'dimensions' must be a vector of positive integers", call. = FALSE)
  }
  
  # Create appropriate structure based on type
  db <- switch(type,
    "array" = {
      if(verbose) cat("Creating array with dimensions:", paste(dimensions, collapse = "x"), "\n")
      array(NA_real_, dim = dimensions)
    },
    "vector" = {
      if(verbose) cat("Creating vector of length:", dimensions[1], "\n")
      rep(NA_real_, dimensions[1])
    },
    "matrix" = {
      if(verbose) cat("Creating matrix with dimensions:", paste(dimensions[1:2], collapse = "x"), "\n")
      matrix(NA_real_, nrow = dimensions[1], ncol = dimensions[2])
    },
    "neighborhood memories" = {
      # Validate dimensions for neighborhood memories
      expected_cols <- 1 + 4 * dimensions[3] + (dimensions[4] - 1) * dimensions[3] + 
                      dimensions[4] * dimensions[3]
      if(dimensions[2] != expected_cols) {
        stop(sprintf("Invalid column dimension. Expected %d columns.", expected_cols),
             call. = FALSE)
      }
      
      if(verbose) {
        cat("Creating neighborhood memories structure:\n")
        cat("Rows:", dimensions[1], "\n")
        cat("Columns:", dimensions[2], "\n")
        cat("Neighbors:", dimensions[3], "\n")
        cat("Components:", dimensions[4], "\n")
      }
      
      # Create and name columns
      db <- as.data.frame(matrix(NA_real_, nrow = dimensions[1], ncol = dimensions[2]))
      colnames(db) <- c(
        "i",
        rep("nn-times", dimensions[3]),
        rep("nn-dists", dimensions[3]),
        rep("nn-weights", dimensions[3]),
        rep("nn-patt", dimensions[3]),
        paste(rep(paste("Sig-Comp.", 1:(dimensions[4] - 1)), dimensions[3]),
              rep(1:dimensions[3], each = dimensions[4] - 1),
              sep = " of NN"),
        paste(rep(paste("Coord.", 1:dimensions[4]), dimensions[3]),
              rep(1:dimensions[3], each = dimensions[4]),
              sep = " of NN")
      )
      db
    },
    stop("Invalid type specified", call. = FALSE)
  )
  
  return(db)
}
