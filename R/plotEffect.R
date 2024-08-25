#' @title Visualize Pattern Causality Effects
#' 
#' @description
#' The `plotEffect` function generates a plot to visualize the effects of positive, negative, or dark causality based on the results from the `pcEffect` function. It displays the influence exerted versus influence received for each item, providing a clear graphical representation of the causality effects.
#' 
#' @importFrom plot3D text2D scatter2D
#' @importFrom corrplot COL1
#' @param effects A list containing data frames of causality effects for positive, negative, and dark influences, as returned by the `pcEffect` function. Each data frame includes columns for received influence, exerted influence, and the difference between them.
#' @param status A character string specifying which causality effect to plot. Must be one of `"positive"`, `"negative"`, or `"dark"`.
#' @param addlabel A logical value indicating whether to add labels to the plot. Defaults to `TRUE`.
#' 
#' @return
#' A scatter plot visualizing the relationship between influence exerted and influence received for the specified causality type. The plot also includes color coding to represent the difference in influence.
#' 
#' @export
#' @examples
#' \dontrun{
#' data(climate)
#' dataset <- climate[,-1]
#' pcmatrix <- pcMatrix(dataset, E = 3, tau = 1, metric = "euclidean", h = 1, weighted = TRUE)
#' effects <- pcEffect(pcmatrix)
#' plotEffect(effects, status = "positive", addlabel = TRUE)
#' }

plotEffect <- function(effects, status,addlabel=TRUE){
  if(typeof(status)!="character"){
    stop("Please enter the correct status with string format.")
  }
  else if(!is.element(status,c("positive","negative","dark"))){
    stop("Please enter the parameter in 'positive','negative' and 'dark'.")
  }
  if(status == "positive"){
    mcolor <- "Blues"
  }
  else if(status == "negative"){
    mcolor <- "Reds"
  }
  else{
    mcolor <- "Purples"
  }
  with(effects, text2D(x = c(effects[status][[1]][2])[[1]], y = c(effects[status][[1]][1]+max(effects[status][[1]][1])*0.01)[[1]], colvar = c(effects[status][[1]][3])[[1]], 
                         xlab = "Influence exerted", ylab = "Influence received", ylim=c(min(effects[status][[1]][1])-1,max(effects[status][[1]][1])+1+max(effects[status][[1]][1])*0.01),labels = rownames(effects[status][[1]]), cex = 0.6, 
                         adj = 0.5, font = 2, col=ramp.col(col = COL1(mcolor), n = 100, alpha = 1)))
  with(effects, scatter2D(x = c(effects[status][[1]][2])[[1]], y = c(effects[status][[1]][1])[[1]], colvar = c(effects[status][[1]][3])[[1]], 
                            pch = 16, add = addlabel, xlab = "Influence exerted", ylab = "Influence received", colkey = TRUE, col=ramp.col(col = COL1(mcolor), n = 100, alpha = 1)))
}
