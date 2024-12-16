test_that("compute_spaces works with default methods", {
  data(climate_indices)
  X <- climate_indices$AO[1:100]
  Y <- climate_indices$AAO[1:100]
  
  spaces <- compute_spaces(X, Y, E = 3, tau = 2, metric = "euclidean")
  
  expect_type(spaces, "list")
  expect_named(spaces, c("Mx", "My", "SMx", "SMy", "PSMx", "PSMy", "Dx", "Dy"))
  expect_true(all(sapply(spaces, is.matrix)))
})

test_that("compute_spaces works with custom methods", {
  data(climate_indices)
  X <- climate_indices$AO[1:100]
  Y <- climate_indices$AAO[1:100]
  
  # Custom state space method
  custom_space <- function(x, E, tau) {
    n <- length(x) - (E-1)*tau
    mat <- matrix(NA, nrow = n, ncol = E)
    for(i in 1:E) {
      mat[,i] <- x[1:n + (i-1)*tau]
    }
    list(matrix = mat)
  }
  
  # Custom distance metric
  custom_dist <- function(x) {
    as.matrix(dist(x, method = "manhattan"))
  }
  
  spaces <- compute_spaces(X, Y, E = 3, tau = 2, metric = "euclidean",
                         state_space_fn = custom_space,
                         distance_fn = custom_dist)
  
  expect_type(spaces, "list")
  expect_named(spaces, c("Mx", "My", "SMx", "SMy", "PSMx", "PSMy", "Dx", "Dy"))
  expect_true(all(sapply(spaces, is.matrix)))
})

test_that("compute_spaces works with default parameters", {
  data(climate_indices)
  X <- climate_indices$AO[1:100]
  Y <- climate_indices$AAO[1:100]
  
  # Test with default parameters
  spaces <- compute_spaces(X, Y, E = 3, tau = 2, metric = "euclidean")
  
  # Check structure
  expect_type(spaces, "list")
  expect_named(spaces, c("Mx", "My", "SMx", "SMy", "PSMx", "PSMy", "Dx", "Dy"))
  
  # Check dimensions
  expect_equal(ncol(spaces$Mx), 3)  # E = 3
  expect_equal(ncol(spaces$My), 3)  # E = 3
  
  # Check data types
  expect_true(is.matrix(spaces$Mx))
  expect_true(is.matrix(spaces$My))
  expect_true(is.matrix(spaces$Dx))
  expect_true(is.matrix(spaces$Dy))
}) 