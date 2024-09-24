#' @title Visualize Pattern Causality Matrix
#' 
#' @description
#' The `plotMatrix` function generates a visual representation of a pattern causality matrix using different methods. It allows users to visualize the positive, negative, or dark causality effects in a specified matrix, providing insight into the relationships between items.
#' 
#' @importFrom corrplot corrplot COL1
#' @param pcmatrix A list containing three matrices (`positive`, `negative`, and `dark`) which represent the respective causality types for different items.
#' @param status A character string specifying which causality matrix to plot. Must be one of `"positive"`, `"negative"`, or `"dark"`.
#' @param method A character string specifying the visualization method for the plot. Options include `"circle"`, `"square"`, `"ellipse"`, `"number"`, `"shade"`, `"color"`, and `"pie"`.
#' 
#' @return
#' A visual plot of the selected causality matrix using the specified method. The plot provides a color-coded representation of the causality strengths between items.
#' 
#' @export
#' @examples
#' \donttest{
#' data(climate_indices)
#' dataset <- climate_indices[,-1]
#' pcmatrix <- pcMatrix(dataset, E = 3, tau = 1, metric = "euclidean", h = 1, weighted = TRUE)
#' plotMatrix(pcmatrix, status = "positive", method = "color")
#' }
plotMatrix <- function(pcmatrix,status,method){
  if(!method %in% c('circle', 'square', 'ellipse', 'number', 'shade', 'color', 'pie')){
    stop("Please enter the correct method.")
  }
  else if(typeof(status)!="character"){
    stop("Please enter the correct status with string format.")
  }
  else if(!is.element(status,c("positive","negative","dark"))){
    stop("Please enter the parameter in 'positive','negative' and 'dark'.")
  }
  cdata <- pcmatrix[status][[1]]
  if(status == "positive"){
    mcolor <- "Blues"
  }
  else if(status == "negative"){
    mcolor <- "Reds"
  }
  else{
    mcolor <- "Purples"
  }
  cdata[is.na(cdata)] <- 0
  cdata <- cdata*100
  if(mcolor == "Purples"){
    corrplot(cdata,is.corr = FALSE, method, addrect = 2, tl.pos='n',addgrid.col = NA, col=COL1(mcolor)[0:160], col.lim=c(0,max(cdata)+(5-max(cdata)%%5)))
  }
  else{
    corrplot(cdata,is.corr = FALSE, method, addrect = 2, tl.pos='n',addgrid.col = NA, col=COL1(mcolor), col.lim=c(0,max(cdata)+(5-max(cdata)%%5)))
  }
}
