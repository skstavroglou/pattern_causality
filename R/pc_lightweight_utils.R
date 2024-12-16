# Internal helper functions
initialize_components <- function(E, tau) {
  list(
    NNSPAN = E + 1,
    CCSPAN = (E - 1) * tau,
    hashedpatterns = patternHashing(E)$hashes
  )
}

compute_causality_measures <- function(results, weighted) {
  # Ensure real_loop exists and is not empty
  if (is.null(results$real_loop) || length(results$real_loop) == 0) {
    return(list(
      total = NA_real_,
      positive = NA_real_,
      negative = NA_real_,
      dark = NA_real_
    ))
  }
  
  # Only use valid points in real_loop
  valid_indices <- which(results$noCausality != 1)
  
  # Calculate total causality
  total_causality <- 1-mean(results$noCausality,na.rm = TRUE)
  
  # Calculate component measures
  positive <- mean(results$Positive[valid_indices], na.rm = TRUE)
  negative <- mean(results$Negative[valid_indices], na.rm = TRUE)
  dark <- mean(results$Dark[valid_indices], na.rm = TRUE)
  
  # Normalize (if needed)
  if (weighted && !anyNA(c(positive, negative, dark))) {
    total <- sum(c(positive, negative, dark))
    if (total > 0) {
      positive <- positive / total
      negative <- negative / total
      dark <- dark / total
    }
  }
  
  # Ensure all values are finite
  if (!all(is.finite(c(total_causality, positive, negative, dark)))) {
    warning("Some causality measures are not finite")
  }
  
  list(
    total = total_causality,
    positive = positive,
    negative = negative,
    dark = dark
  )
}

#' Analyze Pattern Causality Between Time Series
#' 
#' @title Pattern Causality Analysis
#' @description Internal function that performs the main causality analysis loop
#' by analyzing patterns and signatures in reconstructed state spaces.
#'
#' @param spaces List containing state and pattern spaces
#' @param matrices List containing initialized analysis matrices
#' @param components List containing analysis components
#' @param check List containing causality check results
#' @param h Integer; prediction horizon
#' @param weighted Logical; whether to use weighted calculations
#' @param verbose Logical; whether to display progress information
#'
#' @return A list containing:
#'   \itemize{
#'     \item noCausality: Vector of no causality indicators
#'     \item Positive: Vector of positive causality strengths
#'     \item Negative: Vector of negative causality strengths
#'     \item Dark: Vector of dark causality strengths
#'   }
#'
#' @keywords internal
#' @noRd
analyze_causality <- function(spaces, matrices, components, check, h, weighted, verbose) {
  
  # Initialize results
  results <- list(
    noCausality = rep(NA_real_, length(check$al_loop_dur)),
    Positive = rep(NA_real_, length(check$al_loop_dur)),
    Negative = rep(NA_real_, length(check$al_loop_dur)),
    Dark = rep(NA_real_, length(check$al_loop_dur))
  )
  
  real_loop <- numeric(0)
  
  # Main analysis loop
  for(i in seq_along(check$al_loop_dur)) {
    current_point <- check$al_loop_dur[i]
    
    if(!anyNA(c(spaces$Mx[current_point,], spaces$My[current_point + h,]))) {
      nn_info <- pastNNsInfo(
        CCSPAN = components$CCSPAN,
        NNSPAN = components$NNSPAN,
        Mx = spaces$Mx,
        Dx = spaces$Dx,
        SMx = spaces$SMx,
        PSMx = spaces$PSMx,
        i = current_point,
        h = h
      )

      if(!anyNA(nn_info$dists) && !anyNA(spaces$Dy[current_point, nn_info$times + h])) {
        real_loop <- c(real_loop, current_point)
        
        proj_info <- projectedNNsInfo(
          My = spaces$My,
          Dy = spaces$Dy,
          SMy = spaces$SMy,
          PSMy = spaces$PSMy,
          timesX = nn_info$times,
          i = current_point,
          h = h
        )
        
        # Update matrices
        matrices <- update_matrices(
          matrices, spaces, nn_info, proj_info,
          current_point, h, weighted, verbose, components$hashedpatterns
        )
      }
    }
    
    if(verbose) {
      report_progress(i, length(check$al_loop_dur), "Analyzing causality patterns", verbose)
    }
  }
  
  if(verbose) {
    cat("\nComputing final results...\n")
  }
  
  # Calculate causality spectrum
  if(length(real_loop) > 0) {
    spectrums <- compute_causality_spectrums(matrices$pc_matrices, real_loop, components$hashedpatterns, spaces$Mx[,1])

    # Update results
    for(i in seq_along(real_loop)) {
      idx <- which(check$al_loop_dur == real_loop[i])
      results$noCausality[idx] <- spectrums$predicted$no_causality[i]
      results$Positive[idx] <- spectrums$predicted$positive[i]
      results$Negative[idx] <- spectrums$predicted$negative[i]
      results$Dark[idx] <- spectrums$predicted$dark[i]
    }
  }
  
  results$real_loop <- real_loop
  results
}
