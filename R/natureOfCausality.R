#' @title Determine Nature of Causality
#'
#' @description This function analyzes a three-dimensional pattern causality matrix to classify the nature of causality (positive, negative, dark, or no causality) between pairs of variables across specified time points. It is designed to interpret the dynamics within complex systems by examining the causal relationships encoded in the matrix.
#'
#' @param PC A three-dimensional array where each slice along the third dimension represents a pattern causality matrix at a specific time point, encoding the strength and type of causality between pairs of variables.
#' @param dur A numeric vector indicating the time points (slices of the PC matrix) to analyze for causality.
#' @param hashedpatterns A numeric vector of hashed or indexed pattern identifiers that correspond to variables in the system, used for interpreting matrix dimensions in causality checks.
#' @param X An auxiliary numeric vector used to determine the length of the output vectors for causality results, typically aligning with the number of time points or variables.
#'
#' @return A data frame with four columns: 'noCausality', 'Positive', 'Negative', and 'Dark', each containing binary indicators (1 for presence, 0 for absence) that correspond to the presence of each causality type at each time point analyzed.
#' @export
#'
#' @examples
#' # Generate a sample 3D causality matrix with random data
#' set.seed(123) # For reproducibility
#' PC <- array(runif(300), dim = c(10, 10, 3)) # 10x10 matrix over 3 time points
#' dur <- 1:3 # Time points to analyze
#' hashedpatterns <- seq(1, 10) # Simulated hashed pattern identifiers
#' X <- rep(0, 3) # Auxiliary vector for output length
#'
#' # Run the natureOfCausality function
#' results <- natureOfCausality(PC, dur, hashedpatterns, X)
#' print(results)
natureOfCausality <- function(PC, dur, hashedpatterns, X) {
  positiveCausality <- vector(mode = "double", length = length(X))
  negativeCausality <- vector(mode = "double", length = length(X))
  darkCausality <- vector(mode = "double", length = length(X))
  noCausality <- vector(mode = "double", length = length(X))
  # positiveCausality <- rep(NA, len)
  # negativeCausality <- rep(NA, len)
  # darkCausality <- rep(NA, len)
  # noCausality <- rep(NA, len)
  for (i in dur) {
    cell <- which(!is.na(PC[, , i]), arr.ind = TRUE)
    # print(paste("FOR",i))
    if (!is.na(PC[cell[1], cell[2], i])) {
      # print("Inside IF 1")
      if (!is.nan(PC[cell[1], cell[2], i])) {
        # print("Inside IF 2")
        # if (PC[cell[1],cell[2],i]!=0) {
        if (!is.na(cell[1])) {
          # print("Inside IF 3")
          if (!is.na(cell[2])) {
            # print("Inside IF 4")
            # ======================#
            # = POSITIVE CAUSALITY =#
            # ======================#
            if (cell[1] == cell[2]) {
              if (cell[1] != mean(1:length(hashedpatterns))) {
                ### NO CAUSALITY CHECK
                if (PC[cell[1], cell[2], i] == 0) {
                  noCausality[i] <- 1
                  positiveCausality[i] <- 0
                } else {
                  positiveCausality[i] <- 1
                  noCausality[i] <- 0
                }
                negativeCausality[i] <- 0
                darkCausality[i] <- 0
              } else { # STO KENTRO TOY PC MATRIX
                ### NO CAUSALITY CHECK
                if (PC[cell[1], cell[2], i] == 0) {
                  noCausality[i] <- 1
                  darkCausality[i] <- 0 # PC[cell[1],cell[2],i]
                } else {
                  noCausality[i] <- 0
                  darkCausality[i] <- 1 # PC[cell[1],cell[2],i]
                }
                negativeCausality[i] <- 0
                positiveCausality[i] <- 0
              }
            }
            # ======================#
            # = NEGATIVE CAUSALITY =#
            # ======================#
            else if ((cell[1] + cell[2]) == (length(hashedpatterns) + 1)) {
              if (cell[1] != mean(1:length(hashedpatterns))) {
                ### NO CAUSALITY CHECK
                if (PC[cell[1], cell[2], i] == 0) {
                  noCausality[i] <- 1
                  negativeCausality[i] <- 0 # PC[cell[1],cell[2],i]
                } else {
                  noCausality[i] <- 0
                  negativeCausality[i] <- 1 # PC[cell[1],cell[2],i]
                }
                positiveCausality[i] <- 0
                darkCausality[i] <- 0
              } else {
                ### NO CAUSALITY CHECK
                if (PC[cell[1], cell[2], i] == 0) {
                  noCausality[i] <- 1
                  darkCausality[i] <- 0 # PC[cell[1],cell[2],i]
                } else {
                  noCausality[i] <- 0
                  darkCausality[i] <- 1 # PC[cell[1],cell[2],i]
                }
                negativeCausality[i] <- 0
                positiveCausality[i] <- 0
              }
            }
            # ==================#
            # = DARK CAUSALITY =#
            # ==================#
            else {
              ### NO CAUSALITY CHECK
              if (PC[cell[1], cell[2], i] == 0) {
                noCausality[i] <- 1
                darkCausality[i] <- 0 # PC[cell[1],cell[2],i]
              } else {
                noCausality[i] <- 0
                darkCausality[i] <- 1 # PC[cell[1],cell[2],i]
              }
              negativeCausality[i] <- 0
              positiveCausality[i] <- 0
            }
          }
        }
        # }
      }
    }
  }
  NOC <- data.frame(
    noCausality = noCausality,
    Positive = positiveCausality,
    Negative = negativeCausality,
    Dark = darkCausality
  )
  return(NOC)
}
