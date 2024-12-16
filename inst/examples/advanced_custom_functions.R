#' Advanced Custom Functions Examples
#' 
#' This file demonstrates advanced usage of custom functions in pattern causality analysis

library(patterncausality)
data(climate_indices)

# 1. Improved Adaptive Distance Metric
adaptive_distance <- function(x) {
  if(!is.matrix(x)) x <- as.matrix(x)
  # Ensure no NA values
  if(any(is.na(x))) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  # Select distance metric based on data distribution
  if(sd(x, na.rm = TRUE) > 100) {
    x_scaled <- scale(x)
    d <- as.matrix(dist(x_scaled, method = "euclidean"))
  } else {
    d <- as.matrix(dist(x, method = "manhattan"))
  }
  # Ensure distance matrix is symmetric with zero diagonal
  d[is.na(d)] <- 0
  d <- (d + t(d))/2
  diag(d) <- 0
  return(d)
}

# 2. Improved State Space Reconstruction
denoised_state_space <- function(x, E, tau) {
  # Use median filtering for denoising
  n <- length(x)
  x_smooth <- x
  window <- 3
  for(i in (window+1):(n-window)) {
    x_smooth[i] <- median(x[(i-window):(i+window)])
  }
  
  # Construct state space
  n_out <- length(x_smooth) - (E-1)*tau
  mat <- matrix(NA, nrow = n_out, ncol = E)
  for(i in 1:E) {
    mat[,i] <- x_smooth[1:n_out + (i-1)*tau]
  }
  
  # Ensure no NA values
  if(any(is.na(mat))) {
    mat[is.na(mat)] <- mean(mat, na.rm = TRUE)
  }
  
  list(matrix = mat)
}

# Example 1: Using only adaptive distance metric
result1 <- pcLightweight(
  X = climate_indices$AO[1:200],
  Y = climate_indices$AAO[1:200],
  E = 3,
  tau = 2,
  metric = "euclidean",
  distance_fn = adaptive_distance,
  h = 1,
  weighted = TRUE
)
print("Result with adaptive distance:")
print(result1)

# Example 2: Using only improved state space
result2 <- pcLightweight(
  X = climate_indices$AO[1:200],
  Y = climate_indices$AAO[1:200],
  E = 3,
  tau = 2,
  metric = "euclidean",
  state_space_fn = denoised_state_space,
  h = 1,
  weighted = TRUE
)
print("\nResult with denoised state space:")
print(result2)

# Compare results
compare_results <- data.frame(
  Method = c("Adaptive Distance", "Denoised Space"),
  Total = c(result1$total, result2$total),
  Positive = c(result1$positive, result2$positive),
  Negative = c(result1$negative, result2$negative),
  Dark = c(result1$dark, result2$dark)
)
print("\nComparison of different approaches:")
print(compare_results) 