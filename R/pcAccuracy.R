#' @title Calculate Pattern Causality Accuracy
#' @description This function evaluates the causality prediction accuracy across multiple time series within a dataset using the PC Mk. II Light method. It checks the feasibility of causality analysis between all pairs, computes different types of causality (total, positive, negative, dark), and aggregates these results.
#' @param dataset A matrix or data frame where each column represents a time series.
#' @param E The embedding dimension used for state space reconstruction.
#' @param tau The time delay used in state space reconstruction.
#' @param metric A character string specifying the distance metric to be used (e.g., 'euclidean', 'maximum').
#' @param h Prediction horizon, indicating how far ahead in the time series the predictions are aimed.
#' @param weighted A logical indicating whether to use a weighted approach in calculating the causality strengths.
#' @return A data frame with the embedding parameters and average values of total, positive, negative, and dark causality across all time series pairs in the dataset.
#' @export
#' @examples
#' \donttest{
#' data(climate_indices)
#' data <- climate_indices[, -1]
#' results <- pcAccuracy(data, E = 3, tau = 1, metric = "euclidean", h = 1, weighted = TRUE)
#' print(results)
#' }
pcAccuracy <- function(dataset, E, tau, metric, h, weighted) {
  dataset <- as.matrix(dataset)
  # STORAGE ARRAYS
  couplingsTotal <- dataBank(type = "matrix", dimensions = c(ncol(dataset), ncol(dataset)))
  couplingsPosi <- dataBank(type = "matrix", dimensions = c(ncol(dataset), ncol(dataset)))
  couplingsNega <- dataBank(type = "matrix", dimensions = c(ncol(dataset), ncol(dataset)))
  couplingsDark <- dataBank(type = "matrix", dimensions = c(ncol(dataset), ncol(dataset)))
  # DEPLOY PC MK. II LIGHT Version
  # pb <- tkProgressBar(title = "Still Running on Coal :P", min = 0,
  #                    max = ncol(dataset), width = 300)
  for (i in 1:ncol(dataset)) {
    for (j in 1:ncol(dataset)) {
      if (i != j) {
        if (firstCausalityPointCHECK(E, tau, h, dataset[, i])) {
          if (firstCausalityPointCHECK(E, tau, h, dataset[, j])) {
            # temp <- PC.Mk.II.For.Network.Lite(dataset[,i],dataset[,j],
            #                                  E,tau,metric,h,weighted,comCoordRemoval)
            temp <- pcLightweight(dataset[, i], dataset[, j], E, tau, metric, h, weighted, tpb=FALSE)
            couplingsTotal[i, j] <- temp$total
            couplingsPosi[i, j] <- temp$positive
            couplingsNega[i, j] <- temp$negative
            couplingsDark[i, j] <- temp$dark
          }
        }
      }
    }
    # setTkProgressBar(pb, i, label=paste( i/ncol(dataset)*100, 0),
    #                 "% towards Arc Reactor")
  }
  return(data.frame(
    E = E, tau = tau, total = mean(couplingsTotal, na.rm = T),
    positive = mean(couplingsPosi, na.rm = T),
    negative = mean(couplingsNega, na.rm = T),
    dark = mean(couplingsDark, na.rm = T)
  ))
}
