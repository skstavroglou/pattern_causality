#' @title Optimal Parameters Search for Causality Analysis
#' @description Searches for the optimal embedding dimension (E) and time delay (tau) to maximize the accuracy of causality predictions in a dataset. It evaluates each combination of E and tau for their ability to predict different types of causality: total, positive, negative, and dark.
#' @param Emax The maximum embedding dimension to test.
#' @param tauMax The maximum time delay to test.
#' @param metric The distance metric to use in the causality analysis (e.g., 'euclidean').
#' @param dataset A matrix where each column represents a time series to be analyzed.
#' @return A data frame summarizing the causality analysis results across all tested E and tau values, showing the mean total, positive, negative, and dark causality accuracies for each parameter combination.
#' @export
#' @examples
#' data(climate)
#' dataset <- climate[, -1]
#' \donttest{
#' optimalParams <- optimalParametersSearch(Emax=3, tauMax=3, metric="euclidean", dataset=dataset)
#' print(optimalParams)
#' }
optimalParametersSearch <- function(Emax, tauMax, metric, dataset) {
  if (Emax < 3) {
    stop("Please enter the Emax with the number > 2")
  }
  E_array <- 2:Emax
  tau_array <- 1:tauMax
  testsTotal <- matrix(data = NA, nrow = length(E_array), ncol = length(tau_array))
  testsPosi <- matrix(data = NA, nrow = length(E_array), ncol = length(tau_array))
  testsNega <- matrix(data = NA, nrow = length(E_array), ncol = length(tau_array))
  testsDark <- matrix(data = NA, nrow = length(E_array), ncol = length(tau_array))
  start.time <- Sys.time()
  # pb <- tkProgressBar(title = "Still Running on Coal :P", min = 0,
  #                    max = max(E_array), width = 300)
  for (E in E_array) {
    message(paste("Testing | E: ", E))
    for (tau in tau_array) {
      message(paste("Testing | tau: ", tau))
      temp <- pcAccuracy(
        dataset = dataset,
        E = E, tau = tau, metric = metric, h = 0, FALSE
      )
      testsTotal[E - 1, tau] <- temp$total
      testsPosi[E - 1, tau] <- temp$positive
      testsNega[E - 1, tau] <- temp$negative
      testsDark[E - 1, tau] <- temp$dark
    }
    # setTkProgressBar(pb, E, label=paste( E/max(E_array)*100, 0),
    #                 "% towards Arc Reactor")
  }
  # = MANIPULATING THE RESULTS
  accuracyPerE <- list()
  for (E in 2:Emax) {
    accuracyPerE[[E - 1]] <- cbind(
      t(testsTotal)[, E - 1],
      t(testsPosi)[, E - 1],
      t(testsNega)[, E - 1],
      t(testsDark)[, E - 1]
    )
    colnames(accuracyPerE[[E - 1]]) <- c("Total", "of which Positive", "of which Negative", "of which Dark")
    rowLABELS <- vector()
    for (tau in 1:tauMax) {
      rowLABELS <- c(rowLABELS, c(paste("E =", E, "tau =", tau)))
    }
    rownames(accuracyPerE[[E - 1]]) <- rowLABELS
  }
  # = ACCURACY SUMMARY
  accuracySummary <- accuracyPerE[[1]]
  for (i in 2:length(accuracyPerE)) {
    accuracySummary <- rbind(accuracySummary, accuracyPerE[[i]])
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  message(paste("Calculation duration: ", time.taken))
  return(accuracySummary)
}

zero_counter <- function(vec) {
  return(length(which(vec == 0)))
}
zero_filtering <- function(vec, threshold) {
  return(zero_counter(vec) < threshold)
}
na_counter <- function(vec) {
  return(length(which(is.na(vec))))
}
na_filtering <- function(vec, threshold) {
  return(na_counter(vec) < threshold)
}
