#' @title Calculate Pattern Causality Effect
#' 
#' @description
#' The `pcEffect` function processes a pattern causality matrix to compute and summarize the effects of positive, negative, and dark causality. It aggregates these causality measures to determine the total received and exerted influence for each item in the matrix, along with the difference between them.
#' 
#' @param pcmatrix A list containing three matrices (`positive`, `negative`, and `dark`) which represent the respective causality types for different items, as well as an `items` vector indicating the names of the items.
#' 
#' @return
#' A list containing three data frames (`positive`, `negative`, and `dark`), each summarizing the causality effects. Each data frame includes columns for the received, exerted, and the difference (`Diff`) of causality for each item.
#' 
#' @export
#' @examples
#' \donttest{
#' data(climate_indices)
#' dataset <- climate_indices[,-1]
#' pcmatrix <- pcMatrix(dataset, E = 3, tau = 1, metric = "euclidean", h = 1, weighted = TRUE)
#' effects <- pcEffect(pcmatrix)
#' print(effects)
#' }
pcEffect <- function(pcmatrix){
  pdata <- pcmatrix$positive
  ndata <- pcmatrix$negative
  ddata <- pcmatrix$dark
  pdata[is.na(pdata)] <- 0
  pdata <- pdata*100
  ndata[is.na(ndata)] <- 0
  ndata <- ndata*100
  ddata[is.na(ddata)] <- 0
  ddata <- ddata*100
  psume <- apply(pdata,1,sum)
  psumr <- apply(pdata,2,sum)
  pd <- data.frame(psume,psumr,psume-psumr,row.names = pcmatrix$items)
  colnames(pd) <- c("received","exerted","Diff")
  nsume <- apply(ndata,1,sum)
  nsumr <- apply(ndata,2,sum)
  nd <- data.frame(nsume,nsumr,nsume-nsumr,row.names = pcmatrix$items)
  colnames(nd) <- c("received","exerted","Diff")
  dsume <- apply(ddata,1,sum)
  dsumr <- apply(ddata,2,sum)
  dd <- data.frame(dsume,dsumr,dsume-dsumr,row.names = pcmatrix$items)
  colnames(dd) <- c("received","exerted","Diff")
  return(list(positive=pd,
              negative=nd,
              dark=dd,
              items=pcmatrix$items))
}
