#' @title Create Pattern Causality Matrix
#' @description This function performs dynamic network analysis to detect causal interactions between multiple time series within a dataset. It uses the Pattern Causality Model Mk. II to evaluate positive, negative, and dark causality relationships by analyzing the reconstructed state spaces of the time series. The function iterates through each time series in the dataset, comparing them to identify potential causal links.
#' @param dataset A data frame where each column represents a time series to be analyzed for causal relationships.
#' @param E An integer specifying the embedding dimension for reconstructing the state space of the time series.
#' @param tau An integer representing the time delay used in reconstructing the time series in the embedded space.
#' @param metric A character string specifying the distance metric to be used in the analysis (e.g., 'euclidean').
#' @param h An integer indicating the prediction horizon, i.e., the number of steps ahead for which predictions are made.
#' @param weighted A logical value indicating whether the analysis should apply a weighted approach when calculating causality strength.
#' @return A list containing three matrices (`positive`, `negative`, `dark`) that represent the detected causal relationships between the time series in the dataset. Each matrix provides the strength of positive, negative, and dark causality, respectively.
#' @export
#' @examples
#' \donttest{
#' data(climate_indices)
#' dataset <- climate_indices[,-1] # remove the date column
#' result <- pcMatrix(dataset, E = 3, tau = 1, metric = "euclidean", h = 2, weighted = TRUE)
#' print(result)
#' }
pcMatrix <- function(dataset,E,tau,metric,h,weighted) {
  # DURATION OF EXPERIMENT
  #FCP <- firstCausalityPoint(E,tau,h,dataset[,1])
  #dur <- FCP:(nrow(dataset)-(E-1)*tau-h)
  # STORAGE ARRAYS
  couplingsPosi <- dataBank(type = "array",dimensions=c(ncol(dataset),ncol(dataset)))
  couplingsNega <- dataBank(type = "array",dimensions=c(ncol(dataset),ncol(dataset)))
  couplingsDark <- dataBank(type = "array",dimensions=c(ncol(dataset),ncol(dataset)))
  # DEPLOY PC MK. II LIGHT Version
  start.time <- Sys.time()
  #pb <- tkProgressBar(title = "Still Running on Coal :P", min = 0,
  #                    max = ncol(dataset), width = 300)
  for (i in 1:ncol(dataset)) {
    cat(paste("CAUSE: ", colnames(dataset)[i]), "\n")
    for (j in 1:ncol(dataset)) {
      cat(paste("EFFECT: ", colnames(dataset)[j]), "\n")
      if (i != j) {
        if (firstCausalityPointCHECK(E,tau,h,dataset[,i])) {
          if (firstCausalityPointCHECK(E,tau,h,dataset[,j])) {
            #temp <- PC.Mk.II.For.Network.Lite(dataset[,i],dataset[,j],
            #                                  E,tau,metric=metric,h,weighted,comCoordRemoval)
            temp <- pcLightweight(dataset[,i],dataset[,j],
                                              E,tau,metric="euclidean",h,weighted, tpb=FALSE)
            #print(paste("temp$Positive: ", length(temp$Positive)))
            #print(paste("nrow dataset: ", nrow(dataset)))
            couplingsPosi[i,j] <- temp$positive
            couplingsNega[i,j] <- temp$negative
            couplingsDark[i,j] <- temp$dark
          }
        }
      }
    }
    #setTkProgressBar(pb, i, label=paste( i/ncol(dataset)*100, 0),
    #                 "% towards Arc Reactor")
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  cat(paste("Calculation duration: ", time.taken))
  return(list(positive=couplingsPosi,
              negative=couplingsNega,
              dark=couplingsDark,
              items=colnames(dataset)))
}