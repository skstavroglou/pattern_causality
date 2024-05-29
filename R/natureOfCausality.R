#' Classify the Nature of Causality from a Pattern Causality Matrix
#'
#' This function analyzes the pattern causality matrix over a given duration to classify the nature of causality 
#' (positive, negative, dark, or no causality) between variables in a dynamic complex system. The function checks 
#' for causal relationships based on the entries in the pattern causality matrix (PC) and categorizes the nature 
#' of these interactions.
#'
#' @title Causality Nature Classifier
#' @description Processes a 3-dimensional pattern causality matrix to identify and classify the nature of causal
#' relationships over a specified duration. It determines if the causality is positive, negative, dark, or nonexistent 
#' based on the matrix values.
#' @param PC Array, a three-dimensional pattern causality matrix where each cell represents the causal influence 
#' from one variable to another over time.
#' @param dur Integer vector, the indices of time steps at which to evaluate causality.
#' @param hashedpatterns Numeric vector, typically an indexed representation of the variables or patterns under analysis.
#' @param len Integer, the expected length of the output vectors for each type of causality.
#' @return A dataframe with four columns: `noCausality`, `Positive`, `Negative`, and `Dark`, each indicating 
#' the presence (1) or absence (0) of the respective type of causality at each time step.
#' @examples
#' # Example usage:
#' PC <- array(runif(100), dim = c(10, 10, 10))
#' dur <- 1:10
#' hashedpatterns <- seq(1, 10)
#' len <- 10
#' result <- natureOfCausality(PC, dur, hashedpatterns, len)
#' @export
natureOfCausality <- function(PC,dur,hashedpatterns,len) {
  # positiveCausality <- vector(mode = "double", length = length(X))
  # negativeCausality <- vector(mode = "double", length = length(X))
  # darkCausality <- vector(mode = "double", length = length(X))
  # noCausality <- vector(mode = "double", length = length(X))
  positiveCausality <- rep(NA, len)
  negativeCausality <- rep(NA, len)
  darkCausality <- rep(NA, len)
  noCausality <- rep(NA, len)
  for(i in dur) {
    cell <- which(!is.na(PC[,,i]), arr.ind = TRUE)
    #print(paste("FOR",i))
    if (!is.na(PC[cell[1],cell[2],i])) {
      #print("Inside IF 1")
      if (!is.nan(PC[cell[1],cell[2],i])) {
        #print("Inside IF 2")
        #if (PC[cell[1],cell[2],i]!=0) {
        if(!is.na(cell[1])) {
          #print("Inside IF 3")
          if(!is.na(cell[2])) {
            #print("Inside IF 4")
            #======================#
            #= POSITIVE CAUSALITY =#
            #======================#
            if(cell[1]==cell[2]) {
              if (cell[1]!=mean(1:length(hashedpatterns))) {
                ### NO CAUSALITY CHECK
                if (PC[cell[1],cell[2],i] == 0) {
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
                if (PC[cell[1],cell[2],i] == 0) {
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
            #======================#
            #= NEGATIVE CAUSALITY =#
            #======================#
            else if ((cell[1]+cell[2])==(length(hashedpatterns)+1)) {
              if (cell[1]!=mean(1:length(hashedpatterns))) {
                ### NO CAUSALITY CHECK
                if (PC[cell[1],cell[2],i] == 0) {
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
                if (PC[cell[1],cell[2],i] == 0) {
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
            #==================#
            #= DARK CAUSALITY =#
            #==================#
            else {
              ### NO CAUSALITY CHECK
              if (PC[cell[1],cell[2],i] == 0) {
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
        #}
      }
    }
  }
  NOC <- data.frame(noCausality=noCausality,
                    Positive=positiveCausality,
                    Negative=negativeCausality,
                    Dark=darkCausality)
  return(NOC)
}