test_that("custom functions work consistently", {
  data(climate_indices)
  X <- climate_indices$AO[1:100]
  Y <- climate_indices$AAO[1:100]
  
  # Custom functions
  custom_dist <- function(x) {
    if(!is.matrix(x)) x <- as.matrix(x)
    as.matrix(dist(x, method = "manhattan"))
  }
  
  custom_space <- function(x, E, tau) {
    n <- length(x) - (E-1)*tau
    mat <- matrix(NA, nrow = n, ncol = E)
    for(i in 1:E) {
      mat[,i] <- x[1:n + (i-1)*tau]
    }
    list(matrix = mat)
  }
  
  # Test with custom functions
  result <- pcLightweight(
    X, Y, E = 3, tau = 2,
    distance_fn = custom_dist,
    state_space_fn = custom_space,
    h = 1, weighted = TRUE
  )
  
  expect_s3_class(result, "pc_fit")
  expect_named(result, c("total", "positive", "negative", "dark"))
  
  # Verify results are reasonable
  expect_true(all(sapply(result, is.numeric)))
  expect_true(all(sapply(result, function(x) x >= 0 && x <= 1)))
})
