#' @title Calculate Lightweight Pattern Causality
#' @description This function implements the Pattern Causality Model Mk. II for lightweight analysis of causal interactions between two time series using pattern and signature spaces. It assesses causality through reconstructed state spaces and hashed pattern analysis.
#' @param X A numeric vector representing the first time series.
#' @param Y A numeric vector representing the second time series.
#' @param E The embedding dimension, which influences the number of dimensions in which the time series is reconstructed for analysis.
#' @param tau The time delay used in reconstructing the time series in the embedded space.
#' @param metric A character string indicating the distance metric to be used (e.g., 'euclidean', 'maximum').
#' @param h The prediction horizon, representing the number of steps ahead for which predictions are needed.
#' @param weighted A logical indicating whether to use a weighted approach in the causality strength calculations.
#' @param tpb A bool parameter to show progress bar.

#' @return A data frame with columns for total, positive, negative, and dark causality percentages across evaluated time points, giving insights into the nature of causality between the time series.
#' @export
#' @examples
#' data(climate_indices)
#' X <- climate_indices$AO
#' Y <- climate_indices$AAO
#' result <- pcLightweight(X, Y, E = 3, tau = 1, metric = "euclidean", h = 2, weighted = TRUE)
#' print(result)
pcLightweight <- function(X, Y, E, tau, metric, h, weighted, tpb=TRUE) {
  ###################################
  ### STEP 0: PREPARATORY ACTIONS ###
  ###################################
  NNSPAN <- E + 1 # Former NN | Reserves a minimum number of nearest neighbors
  CCSPAN <- (E - 1) * tau # This will remove the common coordinate NNs
  hashedpatterns <- patternHashing(E)
  #####################################
  ### STEP 1: THE SHADOW ATTRACTORS ###
  #####################################
  # = [A] =# State Space
  Mx <- stateSpace(X, E, tau)
  My <- stateSpace(Y, E, tau)
  # = [B] =# Signature Space
  SMx <- signatureSpace(Mx, E)
  SMy <- signatureSpace(My, E)
  # = [C] =# Pattern Space
  PSMx <- patternSpace(SMx, E)
  PSMy <- patternSpace(SMy, E)
  # = [D] =# Distance Matrix | First row corresponds to t=1
  Dx <- distanceMatrix(Mx, metric)
  Dy <- distanceMatrix(My, metric)
  # = Check whether time series length is sufficient
  FCP <- firstCausalityPoint(E, tau, h, X)
  # = Calculate the main loop duration of the algorithm
  al_loop_dur <- FCP:(length(X) - (E - 1) * tau - h)
  # = Calculate the loop duration for out of sample forecasts
  out_of_sample_loop_dur <- ((length(X) - (E - 1) * tau - h) + 1):nrow(Mx)
  # = KEEPING THE PC MATRICES | Causality is considered only from FCP onwards
  predictedPCMatrix <- dataBank(type = "array", dimensions = c(3^(E - 1), 3^(E - 1), length(Y)))
  if(tpb==TRUE){pb <- utils::txtProgressBar(min = 0, max = length(al_loop_dur), style = 3, char="#")}
  #pb <- tkProgressBar(title = "Deploying PC Mk. II", min = 0,
  #                    max = length(al_loop_dur), width = 500)
  real_loop <- NA
  for (i in al_loop_dur) {
    if (!anyNA(c(Mx[i, ], My[i + h, ]))) {
      ###################################################################
      ### STEP 2: The Nearest Neighbours and their Future projections ###
      ###################################################################
      NNx <- pastNNsInfo(CCSPAN, NNSPAN, Mx, Dx, SMx, PSMx, i, h)
      if (!anyNA(NNx$dists)) {
        if (!anyNA(Dy[i, NNx$times + h])) {
          if (any(is.na(real_loop))) {
            real_loop <- i
          } else {
            real_loop <- c(real_loop, i)
          }
          projNNy <- projectedNNsInfo(My, Dy, SMy, PSMy, NNx$times, i, h)
          #######################################################################
          ### STEP 3: The affected variable's predicted pattern h steps ahead ###
          #######################################################################
          predictedSignatureY <- predictionY(E, projNNy, zeroTolerance = E - 1)$predictedSignatureY
          predictedPatternY <- predictionY(E, projNNy, zeroTolerance = E - 1)$predictedPatternY[1]
          #############################################
          ### STEP 4: The causal variable's pattern ###
          #############################################
          #################### signatureX <- signaE(E,SignX,i)
          signatureX <- SMx[i, ]
          patternX <- PSMx[i, ]
          ####################################################
          ### STEP 5: The affected variable's real pattern ###
          ####################################################
          ####### realSignatureY <- signaE(E,SignY,(i+h))
          realSignatureY <- SMy[(i + h), ]
          realPatternY <- PSMy[i + h]
          ##########################################################################
          ### STEP 6: The nature and intensity of causality at every time step t ###
          ##########################################################################
          pc <- fillPCMatrix(weighted, predictedPatternY, realPatternY, predictedSignatureY, realSignatureY, patternX, signatureX)
          # print(pc)
          predictedPCMatrix[which(hashedpatterns == patternX), which(hashedpatterns == predictedPatternY), i] <- pc$predicted
        }
      }
    }
    #setTkProgressBar(pb, i, label=paste( i/al_loop_dur[length(al_loop_dur)], 0),"% PC Mk. II In-Sample Assignment Completion")
    if(tpb==TRUE){utils::setTxtProgressBar(pb, i)}
  }
  causality <- natureOfCausality(predictedPCMatrix, real_loop, hashedpatterns, X)
  totalCausPercent <- 1 - mean(causality$noCausality, na.rm = T)
  posiCausPercent <- mean(ifelse(causality$noCausality[real_loop] != 1, causality$Positive[real_loop], NA), na.rm = T)
  negaCausPercent <- mean(ifelse(causality$noCausality[real_loop] != 1, causality$Negative[real_loop], NA), na.rm = T)
  darkCausPercent <- mean(ifelse(causality$noCausality[real_loop] != 1, causality$Dark[real_loop], NA), na.rm = T)
  # return(list(causality,totalCausPercent,posiCausPercent,negaCausPercent,darkCausPercent))
  result <- pc_fit(
    total = totalCausPercent,
    positive = posiCausPercent,
    negative = negaCausPercent,
    dark = darkCausPercent
  )
  
  return(result)
}
