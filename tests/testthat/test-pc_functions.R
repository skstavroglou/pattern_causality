library(testthat)
library(patterncausality)

# Test data setup
test_that("climate_indices data is available", {
  data(climate_indices)
  expect_true(exists("climate_indices"))
  expect_true(is.data.frame(climate_indices))
  expect_true(all(c("AO", "AAO") %in% names(climate_indices)))
})

# Test pcLightweight function
test_that("pcLightweight handles basic cases", {
  data(climate_indices)
  X <- climate_indices$AO
  Y <- climate_indices$AAO
  
  result <- pcLightweight(X, Y, E = 3, tau = 2, 
                         metric = "euclidean", h = 1, 
                         weighted = TRUE)
  
  # Basic structure tests
  expect_s3_class(result, "pc_fit")
  expect_named(result, c("total", "positive", "negative", "dark"))
  
  # Value range tests
  expect_true(all(sapply(result, is.numeric)))
  expect_true(all(sapply(result, function(x) x >= 0 && x <= 1)))
  
  # Sum of components should be approximately 1
  components_sum <- result$positive + result$negative + result$dark
  expect_equal(components_sum, 1, tolerance = 0.1)
})

# Test pcLightweight error handling
test_that("pcLightweight handles errors appropriately", {
  data(climate_indices)
  X <- climate_indices$AO
  Y <- climate_indices$AAO
  
  # Test invalid inputs
  expect_error(pcLightweight(NULL, Y, E = 3, tau = 2, metric = "euclidean", h = 1, weighted = TRUE))
  expect_error(pcLightweight(X, NULL, E = 3, tau = 2, metric = "euclidean", h = 1, weighted = TRUE))
  expect_error(pcLightweight(X, Y, E = -1, tau = 2, metric = "euclidean", h = 1, weighted = TRUE))
  expect_error(pcLightweight(X, Y, E = 3, tau = -1, metric = "euclidean", h = 1, weighted = TRUE))
})

# Test pcFullDetails function
test_that("pcFullDetails works correctly", {
  data(climate_indices)
  X <- climate_indices$AO[1:100]  # Using smaller subset for speed
  Y <- climate_indices$AAO[1:100]
  
  detail <- pcFullDetails(X, Y, E = 2, tau = 1, metric = "euclidean", h = 1, weighted = TRUE)
  
  # Check structure
  expect_type(detail, "list")
  expect_s3_class(detail, "pc_full_details")
  
  # Check required components
  expected_names <- c("backtest_time", "valid_time", "causality_real", 
                     "causality_pred", "state_spaces", "neighbors", 
                     "patterns", "matrices", "predictions", "weighted", "E")
  expect_named(detail, expected_names)
})

# Test different parameter combinations
test_that("Different parameter combinations work", {
  data(climate_indices)
  X <- climate_indices$AO[1:100]  # Using smaller subset for speed
  Y <- climate_indices$AAO[1:100]
  
  # Test different embedding dimensions with all required parameters
  expect_no_error(pcLightweight(X, Y, E = 2, tau = 1, metric = "euclidean", h = 1, weighted = TRUE))
  expect_no_error(pcLightweight(X, Y, E = 3, tau = 1, metric = "euclidean", h = 1, weighted = TRUE))
  
  # Test different time delays
  expect_no_error(pcLightweight(X, Y, E = 2, tau = 1, metric = "euclidean", h = 1, weighted = TRUE))
  expect_no_error(pcLightweight(X, Y, E = 2, tau = 2, metric = "euclidean", h = 1, weighted = TRUE))
  
  # Test different metrics
  expect_no_error(pcLightweight(X, Y, E = 2, tau = 1, metric = "euclidean", h = 1, weighted = TRUE))
  expect_no_error(pcLightweight(X, Y, E = 2, tau = 1, metric = "manhattan", h = 1, weighted = TRUE))
})