#' @title Pattern Causality Mark II: Full Details
#' @description Implements an advanced pattern causality algorithm to explore the causal relationships between two time series datasets. This function reconstructs state spaces, calculates distances, and evaluates causality using predefined metrics and pattern analysis. The methodology supports complex system analysis where traditional linear methods fall short.
#' @param X Numeric vector, the first time series data.
#' @param Y Numeric vector, the second time series data.
#' @param E Integer, the embedding dimension used in the reconstruction of the state space.
#' @param tau Integer, the time delay between data points in the reconstruction.
#' @param metric Character, the distance metric used (e.g., 'euclidean', 'manhattan').
#' @param h Integer, the prediction horizon, specifying the number of steps ahead for causality analysis.
#' @param weighted Logical, specifies if causality strength should be weighted.
#' @return A list containing various outputs including matrices of nearest neighbors, predicted and actual causality matrices, signature matrices, pattern vectors, and diagnostics related to the time series analysis and causality assessment.
#' @export
#' @examples
#' data(climate_indices)
#' X <- climate_indices$AO
#' Y <- climate_indices$AAO
#' result <- pcFullDetails(X, Y, E = 3, tau = 2, metric = "euclidean", h = 1, weighted = TRUE)
#' print(result)
pcFullDetails <- function(X, Y, E, tau, metric, h, weighted) {
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
  # = KEEPING THE PC MATRICES | Causality is considered only from FCP onwards
  predictedPCMatrix <- dataBank(type = "array", dimensions = c(3^(E - 1), 3^(E - 1), length(Y)))
  realPCMatrix <- dataBank(type = "array", dimensions = c(3^(E - 1), 3^(E - 1), length(Y)))
  # = KEEPING THE SIGNATURES
  predictedSignaturesY <- dataBank(type = "matrix", dimensions = c(length(Y), E - 1))
  realSignaturesY <- dataBank(type = "matrix", dimensions = c(length(Y), E - 1))
  causalSignaturesX <- dataBank(type = "matrix", dimensions = c(length(Y), E - 1))
  # = KEEPING THE PATTERNS
  predictedPatternsY <- dataBank(type = "vector", dimensions = length(Y))
  realPatternsY <- dataBank(type = "vector", dimensions = length(Y))
  causalPatternsX <- dataBank(type = "vector", dimensions = length(Y))
  # = KEEPING THE CONVERTED SIGNATURES INTO ACTUAL VALUES
  predictedValuesY <- dataBank(type = "matrix", dimensions = c(length(Y), E))
  colnames(predictedValuesY) <- c("currVal", rep("predVal", E - 1))
  realValuesY <- dataBank(type = "matrix", dimensions = c(length(Y), E))
  colnames(realValuesY) <- c("currVal", rep("futuVal", E - 1))
  # = KEEPING NEIGHBORHOODS DETAILS
  NNsX <- rep(list(), length(Y))
  NNsYproj <- rep(list(), length(Y))
  # pb <- tkProgressBar(title = "Deploying PC Mk. II", min = 0,
  #                    max = length(al_loop_dur), width = 500)
  pb <- utils::txtProgressBar(min = 0, max = length(al_loop_dur), style = 3, char="#")
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
          # = MEMORY LOG
          NNsX[[i]] <- NNx
          NNsYproj[[i]] <- projNNy
          #######################################################################
          ### STEP 3: The affected variable's predicted pattern h steps ahead ###
          #######################################################################
          predictedSignatureY <- predictionY(E, projNNy, zeroTolerance = E - 1)$predictedSignatureY
          predictedPatternY <- predictionY(E, projNNy, zeroTolerance = E - 1)$predictedPatternY[1]
          # = MEMORY LOG
          predictedSignaturesY[i, ] <- predictedSignatureY
          predictedPatternsY[i] <- predictedPatternY
          #############################################
          ### STEP 4: The causal variable's pattern ###
          #############################################
          #################### signatureX <- signaE(E,SignX,i)
          signatureX <- SMx[i, ]
          patternX <- PSMx[i, ]
          # = MEMORY LOG
          causalSignaturesX[i, ] <- signatureX
          causalPatternsX[i] <- patternX
          ####################################################
          ### STEP 5: The affected variable's real pattern ###
          ####################################################
          ####### realSignatureY <- signaE(E,SignY,(i+h))
          realSignatureY <- SMy[(i + h), ]
          realPatternY <- PSMy[i + h]
          # = MEMORY LOG
          realSignaturesY[i, ] <- realSignatureY
          realPatternsY[i] <- realPatternY
          ##########################################################################
          ### STEP 6: The nature and intensity of causality at every time step t ###
          ##########################################################################
          pc <- fillPCMatrix(weighted, predictedPatternY, realPatternY, predictedSignatureY, realSignatureY, patternX, signatureX)
          # print(pc)
          predictedPCMatrix[which(hashedpatterns == patternX), which(hashedpatterns == predictedPatternY), i] <- pc$predicted
          realPCMatrix[which(hashedpatterns == patternX), which(hashedpatterns == predictedPatternY), i] <- pc$real
          ####################################################
          ### STEP * Convert Predicted Signature to Values ###
          ####################################################
          predictedValuesY[i, ] <- convertSignatureToValue(E, tau, Y, i, h, predictedSignatureY)
          realValuesY[i, ] <- Y[seq(from = i + h, to = i + h + (E - 1) * tau, by = tau)]
        }
      }
    }
    # setTkProgressBar(pb, i, label=paste( i/al_loop_dur[length(al_loop_dur)], 0),"% PC Mk. II Assignment Completion")
    utils::setTxtProgressBar(pb, i)
  }
  spectrumOfCausalityReal <- natureOfCausality(realPCMatrix, real_loop, hashedpatterns, X)
  spectrumOfCausalityPredicted <- natureOfCausality(predictedPCMatrix, real_loop, hashedpatterns, X)
  return(list(
    backtestTime = al_loop_dur,
    realValidTime = real_loop,
    spectrumOfCausalityReal = spectrumOfCausalityReal,
    spectrumOfCausalityPredicted = spectrumOfCausalityPredicted,
    Mx = Mx, My = My, NNsX = NNsX, NNsYproj = NNsYproj,
    predSigsY = predictedSignaturesY, predPattY = predictedPatternsY,
    causSigsX = causalSignaturesX, causPattX = causalPatternsX,
    realSigsY = realSignaturesY, realPattY = realPatternsY,
    predPC = predictedPCMatrix, realPC = realPCMatrix,
    predY = predictedValuesY, realY = realValuesY
  ))
}
