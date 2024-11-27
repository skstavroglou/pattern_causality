test_that("pcLightweight works", {
  data(climate_indices)
  X <- climate_indices$AO
  Y <- climate_indices$AAO
  
  result <- pcLightweight(X, Y, E = 3, tau = 2, 
                         metric = "euclidean", h = 1, 
                         weighted = TRUE, tpb = FALSE)
  
  expect_type(result, "list")
  expect_named(result, c("total", "positive", "negative", "dark"))
  expect_true(all(sapply(result, is.numeric)))
  expect_true(all(sapply(result, function(x) x >= 0 && x <= 1)))
})