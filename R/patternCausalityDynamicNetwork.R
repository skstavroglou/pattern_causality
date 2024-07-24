#' @title Dynamic Network Pattern Causality
#'
#' @description This function identifies and maps the dynamic causal interactions within complex systems using a robust methodology based on information embedded in reconstructed state spaces. It analyzes temporal relationships across variables in a dataset to establish causality networks, handling both positive and negative influences.
#'
#' @param dataset A numeric matrix representing the time series data of various interacting components within the system.
#' @param E Integer, embedding dimension representing the number of past values influencing the current value.
#' @param tau Integer, the time delay between consecutive observations in the dataset.
#' @param metric Character, the distance metric to be used for calculating interdependencies ('euclidean' by default).
#' @param h Integer, prediction horizon over which the causality is assessed.
#' @param weighted Logical, whether the causality calculation should consider weighted influences based on the strength of interactions.
#' @param comCoordRemoval Logical, specifies if common coordinate effects should be removed, adjusting the analysis for potential confounders.
#'
#' @return A list containing three arrays (positive, negative, dark) representing different types of causal interactions and an item list with variable names.
#' @noRd
#' @examples
#' data(climate)
#' 
#' result <- patternCausalityDynamicNetwork(climate[,-1], E=3, tau=1, metric="euclidean", h=1, weighted=TRUE)
#' print(result)


patternCausalityDynamicNetwork <- function(dataset,E,tau,metric,h,weighted) {
  # DURATION OF EXPERIMENT
  #FCP <- firstCausalityPoint(E,tau,h,dataset[,1])
  #dur <- FCP:(nrow(dataset)-(E-1)*tau-h)
  # STORAGE ARRAYS
  couplingsPosi <- dataBank(type = "array",dimensions=c(ncol(dataset),ncol(dataset),nrow(dataset)-tau))
  couplingsNega <- dataBank(type = "array",dimensions=c(ncol(dataset),ncol(dataset),nrow(dataset)-tau))
  couplingsDark <- dataBank(type = "array",dimensions=c(ncol(dataset),ncol(dataset),nrow(dataset)-tau))
  # DEPLOY PC MK. II LIGHT Version
  start.time <- Sys.time()
  #pb <- tkProgressBar(title = "Still Running on Coal :P", min = 0,
  #                    max = ncol(dataset), width = 300)
  for (i in 1:ncol(dataset)) {
    print(paste("CAUSE: ", colnames(dataset)[i]))
    for (j in 1:ncol(dataset)) {
      print(paste("EFFECT: ", colnames(dataset)[j]))
      if (i != j) {
        if (firstCausalityPointCHECK(E,tau,h,dataset[,i])) {
          if (firstCausalityPointCHECK(E,tau,h,dataset[,j])) {
            #temp <- PC.Mk.II.For.Network.Lite(dataset[,i],dataset[,j],
            #                                  E,tau,metric=metric,h,weighted,comCoordRemoval)
            temp <- PC.Mk.II.Lightweight(dataset[,i],dataset[,j],
                                              E,tau,metric="euclidean",h,weighted)
            print(paste("temp$Positive: ", length(temp$positive)))
            print(paste("nrow dataset: ", nrow(dataset)))
            couplingsPosi[i,j,] <- temp$positive
            couplingsNega[i,j,] <- temp$negative
            couplingsDark[i,j,] <- temp$dark
          }
        }
      }
    }
    #setTkProgressBar(pb, i, label=paste( i/ncol(dataset)*100, 0),
    #                 "% towards Arc Reactor")
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste("Calculation duration: ", time.taken))
  return(list(positive=couplingsPosi,
              negative=couplingsNega,
              dark=couplingsDark,
              items=colnames(dataset)))
}