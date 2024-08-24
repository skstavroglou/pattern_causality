#' @title Pattern Causality Cross-Validation
#' 
#' @description
#' The `pcCrossValidation` function performs cross-validation on time series data to evaluate the robustness of pattern causality measures. It repeatedly samples subsets of the data, applies the pattern causality algorithm, and aggregates the results to provide a comprehensive assessment of the causality metrics.
#' 
#' @param X A numeric vector representing the first time series.
#' @param Y A numeric vector representing the second time series.
#' @param E An integer specifying the embedding dimension for the state space reconstruction.
#' @param tau An integer specifying the time delay for the state space reconstruction.
#' @param metric A character string specifying the distance metric used in the causality computation (e.g., "euclidean").
#' @param h An integer specifying the prediction horizon.
#' @param weighted A logical value indicating whether to apply weighted causality measures.
#' @param numberset A numeric vector specifying the sample sizes for the cross-validation procedure.
#' 
#' @return
#' A data frame containing the aggregated causality metrics across different sample sizes. The data frame includes the positive, negative, and dark causality percentages.
#' 
#' @export
#' @examples
#' \donttest{
#' data(DJS)
#' X <- DJS$X3M
#' Y <- DJS$American.Express
#' numberset <- c(1000,2000,3000,4000)
#' result <- pcCrossValidation(X,Y,3,2,"euclidean",1,FALSE,numberset)
#' print(result)
#' }

pcCrossValidation <- function(X, Y, E, tau, metric, h, weighted, numberset){
  if(!is.atomic(numberset)){
    stop("Please enter the vector of the sample number.")
  }
  if(max(numberset) <= length(X)){
    numbers <- sort(numberset)
    positive <- dataBank("vector",length(numberset))
    negative <- dataBank("vector",length(numberset))
    dark <- dataBank("vector",length(numberset))
    pb <- utils::txtProgressBar(min = 1, max = length(numbers), style = 3, char="#")
    for(i in 1:length(numbers)){
      samplex <- sample(X, numbers[i])
      sampley <- sample(Y,numbers[i])
      positive[i] <- pcLightweight(samplex, sampley, E, tau, metric, h, weighted,tpb=FALSE)$positive
      negative[i] <- pcLightweight(samplex, sampley, E, tau, metric, h, weighted,tpb=FALSE)$negative
      dark[i] <- pcLightweight(samplex, sampley, E, tau, metric, h, weighted,tpb=FALSE)$dark
      together <- data.frame(positive, negative, dark)
      rownames(together) <- numbers
      utils::setTxtProgressBar(pb, i)
    }
  }
  else{
    stop("The sample number is larger than the dataset.")
  }
  return(together)
}


#' @title Plot Cross-Validation Results for Pattern Causality
#' 
#' @description
#' The `plotCV` function generates a plot to visualize the results of cross-validation for pattern causality. It displays the positive, negative, and dark causality strengths across different sample sizes, providing a clear graphical representation of the cross-validation outcomes.
#' 
#' @param pcCV A data frame containing the cross-validation results from the `pcCrossValidation` function. The data frame should include columns for positive, negative, and dark causality strengths, along with the corresponding sample sizes.
#' 
#' @return
#' A plot visualizing the positive, negative, and dark causality strengths across different sample sizes. The plot includes points and lines for each causality type, along with a legend for easy interpretation.
#' 
#' @importFrom graphics points lines legend
#' @export
#' @examples
#' \donttest{
#' data(DJS)
#' X <- DJS$X3M
#' Y <- DJS$American.Express
#' numberset <- c(1000,2000,3000,4000)
#' result <- pcCrossValidation(X,Y,3,2,"euclidean",1,FALSE,numberset)
#' plotCV(result)
#' }
plotCV <- function(pcCV){
  pcCV$number <- rownames(pcCV)
  plot(pcCV$number, pcCV$positive,pch=15,col="DarkTurquoise",ylim=c(0,1),xlab="L",ylab="Causality Strength")
  points(pcCV$number, pcCV$negative,pch=16,col="DeepPink",cex=1)
  points(pcCV$number, pcCV$dark,pch=17,col="RosyBrown",cex=1)
  lines(pcCV$number, pcCV$positive,col="DarkTurquoise",lty=1)
  lines(pcCV$number, pcCV$negative,col="DeepPink",lty=2)
  lines(pcCV$number, pcCV$dark,col="RosyBrown",lty=3)
  legend("topright",0.98,c("positive", "negative","dark"),col=c("DarkTurquoise","DeepPink","RosyBrown"),text.col=c("DarkTurquoise","DeepPink","RosyBrown"),pch=c(15,16,17),lty=c(1,2,3))
}

