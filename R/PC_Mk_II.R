#################################################################
#===============================================================#
#=#############################################################=#
#=#                                                           #=#
#=# Pattern Causality Mark II | NA-Proof (PC Mk. II NA-Proof) #=#
#=#                                                           #=#
#=#############################################################=#
#===============================================================#
#################################################################

#=============#
#= STANDARDS =#
#=============#

#= EMBEDDING DIMENSION
#~~~ E=1 : 1-D | Default time series dimension. No embedding
#~~~ E=2 : 2-D | Phase Space related data objects are 1-column (Vectors)
#~~~ E=3 : 3-D | Phase Space related data objects are 2-columns (matrices)
#~~~ E=n : n-D | Phase Space related data objects are [n-1]-columns (matrices)

#= SYMBOLIC TIME SERIES
#~~~ Symbolization in time series is conducted by considering only E, not tau.

#= PHASE SPACE
#~~~ Phase space reconstruction is conducted by considering both E and tau.

#= CAUSALITY
#= WHEN THE CONDITION FOR CAUSALITY IS FALSE WE RETRIEVE "0" (ZERO)
#= WHEN THERE IS MISSING VALUE WE A PRIORI RETIREVE "NA"
#= At time "i" we calculate causality from X[i] to Y[i+h]
#= SpectrumOfCausalityReal VS Predicted is only about the strength of causality. 
#  Both are calculated only when causality is valid.

#= "h" IS ONLY NEEDED WHEN WE WANT TO PREDICT OUT OF SAMPLE

#= PREDICTIVE HORIZON
#= "h" serves only as a buffer in the possible NNs to allow for prediction it does not affect the synchrony of i and j
#= We will take NNs whose "timesX" when projected by "h" steps ahead will STILL
#= find existing NNS in the projected Neighborhood of Y.

#= WHAT ABOUT "h" and LEAD LAG FORMAT OF X and Y???
#= "h" is irrelevant to lead-lag between X and Y. It's only a buffer variable 
#= that MAY POSSIBLY sacrifice causal strength seeking via NNs in X
#= in order to be able to predict future states of Y.

#= LEAD-EXAMINATION SHOULD BE INITIATED OUTSIDE OF THE PC ALGORITHM

#=============#
#= LIBRARIES =#
#=============#

library(snow)
library(snowfall)
library(tcltk2)
library(moments)

#library(rpud)
#library(rpudplus)

#========================#
#= FOUNDATION | LAYER 0 =#
#========================#

#= Appointing Unique Identifiers to Symbolic Patterns
patternHashing <- function(E) {
  a <- possiblePatterns(E)
  if (!anyNA(a)) {
    hashedpatterns <- apply(a, 1, hashing)
  } else {
    hashedpatterns <- NA
  }
  return(hashedpatterns)
}
#=== Prerequisites
possiblePatterns <- function(E) {
  if (E<=1) {
    p <- NA
  } else {
    p <- as.matrix(expand.grid(rep(list(1:3), E-1)))
  }
  return(p)
}
hashing <- function(vec) {
  hash = 0
  for (i in 1:length(vec)) {
    hash = hash + vec[i]*factorial(i+2)
  }
  return(hash)
}

#= Initializing Data Structures
dataBank <- function(type,dimensions) {
  if (type=="array") {
    db <- array(NA,dim = dimensions)
  } else if (type=="vector") {
    db <- vector(mode = "double",length = dimensions[1])
    db <- rep(NA,length(db))
  } else if (type=="matrix") {
    db <- matrix(data = NA,nrow=dimensions[1],ncol=dimensions[2])
  } else if (type=="neighborhood memories") {
    db <- as.data.frame(matrix(data = NA,nrow=dimensions[1],ncol=dimensions[2]))
    colnames(db) <- c("i",rep("nn-times",dimensions[3]),rep("nn-dists",dimensions[3]),
                      rep("nn-weights",dimensions[3]),rep("nn-patt",dimensions[3]),
                      paste(rep(paste("Sig-Comp.", 1:(dimensions[4]-1)),dimensions[3]),
                            rep(1:dimensions[3], each = dimensions[4]-1),
                            sep = " of NN"),
                      paste(rep(paste("Coord.", 1:dimensions[4]),dimensions[3]),
                            rep(1:dimensions[3], each = dimensions[4]),
                            sep = " of NN"))
  } 
  return(db)
}

# First Point for which our algorithm is relevant
firstCausalityPoint <- function(E,tau,h,X) {
  NNSPAN = E+1 # Former NN | Reserves a minimum number of nearest neighbors
  CCSPAN = (E-1)*tau # This will remove the common coordinate NNs
  PredSPAN = h
  FCP = 1 + NNSPAN + CCSPAN + PredSPAN
  if (NNSPAN + CCSPAN + PredSPAN >= length(X)-CCSPAN) {
    stop("The First Point to consider for Causality does not have sufficient 
         Nearest Neighbors. Please Check parameters: 
         E, lag, p as well as the length of X and Y")
  } else {
    FCP = 1 + NNSPAN + CCSPAN + PredSPAN # First Causality Point to be considered
  }
  return(FCP)
}

firstCausalityPointCHECK <- function(E,tau,h,X) {
  NNSPAN = E+1 # Former NN | Reserves a minimum number of nearest neighbors
  CCSPAN = E-1 #(E-1)*tau # This will remove the common coordinate NNs
  PredSPAN = h
  FCP = 1 + NNSPAN + CCSPAN + PredSPAN
  if (NNSPAN + CCSPAN + PredSPAN >= length(X)-CCSPAN) {
    response <- FALSE
  } else {
    response <- TRUE # First Causality Point to be considered
  }
  return(response)
}

#= Utility Function for Signature to Value Conversion
convertSignatureToValue <- function(E,tau,Y,i,h,predictedSignatureY) {
  # The first value is the current
  predictedY <- vector(mode = "double", length = E)
  predictedY[1] <- Y[i+h]
  for (k in 2:E) {
    predictedY[k] <- predictedY[k-1]+predictedSignatureY[k-1]
  }
  return(predictedY)
}
convertSignatureToValueOutOfSample <- function(E,tau,Y_pred_last,i,h,predictedSignatureY) {
  # The first value is the current
  predictedY <- vector(mode = "double", length = E)
  predictedY[1] <- Y_pred_last
  for (k in 2:E) {
    predictedY[k] <- predictedY[k-1]+predictedSignatureY[k-1]
  }
  return(predictedY)
}

#================================================#
#= LAYER 1: PRIMORDIAL STRUCTURES | MACRO STATE =#
#================================================#

#= [A] =# Reconstructing the State Space / Shadow Manifold
stateSpace <- function(ts,E,tau) {
  M <- matrix(NA, length(ts)-E+1-tau+1, E)
  for(i in 1:nrow(M)) {
    M[i,] <- ts[seq(from = i, to = i+ tau*(E-1), by = tau)]
    if (anyNA(M[i,])) {
      M[i,] <- rep(NA,E)
    }
  }
  return(M)
}

#= [B] =# Converting the State Space into Signature Space
signatureSpace <- function(M,E) {
  if (E==2) {
    SM <- as.matrix(apply(M,1,signatureVectorDifference))
  } else if (E>=3) {
    SM <- t(as.matrix(apply(M,1,signatureVectorDifference)))
  }
  return(SM)
}
#=== Prerequisites
signatureVectorDifference <- function(vec) {
  s.vec <- (vec[-1]-vec[-length(vec)])#/vec[-length(vec)]
  #s.vec[is.nan(s.vec)] <- 0
  #s.vec[is.infinite(s.vec)] <- 0
  #s.vec[is.na(s.vec)] <- 0
  return(s.vec)
}

#= [C] =# Converting the Signature Space into Pattern Space
patternSpace <- function(SM,E) {
  PSM <- as.matrix(apply(SM,1,patternVectorDifference,E))
  return(PSM)
}
#=== Prerequisites
patternVectorDifference <- function(sVec,E) {
  if (anyNA(sVec)) {
    p.vec <- as.vector(rep(NA,E-1))
  } else {
    p.vec <- ifelse(sVec>0,3,ifelse(sVec<0,1,2))
  }
  return(hashing(p.vec))
}

#= [D] =# Calculating Distances among all Points
distanceMatrix <- function(M,metric) {
  d <- dist(M,metric, upper=T)
  #d <- as.matrix(dist(M,metric, upper=T))
  return(d)
}

#= Calculating distances for current point ONLY
metricDistance <- function(vec1,vec2,n) {
  res = as.numeric(vec1-vec2)
  distance = (sum(abs(res)^n))^(1/n)
  # print(paste("vec1: ", vec1))
  # print(paste("vec2: ", vec2))
  # if (anyNA(vec1)) {
  #   distance = NA
  # } else if ( anyNA(vec2)) {
  #   distance = NA
  # } else if (!anyNA(vec1)) { 
  #     if (!anyNA(vec2)) {
  #       print(paste("vec1 -vec2: ", as.numeric(vec1-vec2)))
  #       res = as.numeric(vec1-vec2)
  #       distance = (sum(abs(res)^n))^(1/n)
  #     }
  # }
  return( distance )
}
distanceVector <- function(point,candidateNNs,n) {
  return( apply(X = candidateNNs, MARGIN = 1, FUN = metricDistance, vec2=point, n=n) )
}

#=================================================#
#= LAYER 2: NEIGHBORHOODS TOPOLOGY | MICRO STATE =#
#=================================================#

#= Finding Nearest Neighbors and Keeping their Topological Information
pastNNsInfo <- function(CCSPAN,NNSPAN,Mx,Dx,SMx,PSMx,i,h) {
  # REMOVE COMMON COORDINATE VECTORS + horizon FROM CANDIDATE NNs
  candidateNNs <- Dx[i,1:(i-CCSPAN-h)]
  # NEAREST NEIGHBORS OF Y TO PREDICT IN X 
  times <- as.numeric(names(candidateNNs[order(candidateNNs)])[1:NNSPAN])
  dists <- candidateNNs[order(candidateNNs)][1:NNSPAN]
  #= THEIR SIGNATURES
  signatures = SMx[times,]
  #= DELIVERABLE =#
  #                   "weights"=weightsRelativeToDistance(dists),
  thePast <- list("i"=i,"times"=times,"dists"=dists,
                  "signatures"=signatures,"patterns"=PSMx[times],
                  "coordinates"=Mx[times,])
  return(thePast)
}

pastNNsInfo_Lite <- function(CCSPAN,NNSPAN,Mx,Dx,SMx,PSMx,i,h) {
  # REMOVE COMMON COORDINATE VECTORS + horizon FROM CANDIDATE NNs
  candidateNNs <- Dx
  # NEAREST NEIGHBORS OF Y TO PREDICT IN X 
  times <- as.numeric(names(candidateNNs[order(candidateNNs)])[1:NNSPAN])
  dists <- candidateNNs[order(candidateNNs)][1:NNSPAN]
  #cat(paste("At Loop time (i):", i, ".."))
  #cat(paste("times:", times, ".."))
  #cat(paste("dists:", dists, ".."))
  #= THEIR SIGNATURES
  signatures = SMx[times,]
  #= DELIVERABLE =#
  thePast <- list("i"=i,"times"=times,"dists"=dists,
                  "signatures"=signatures,"patterns"=PSMx[times],
                  "coordinates"=Mx[times,])
  return(thePast)
}

#= Projecting Nearest Neighbors into their Future States (Backtesting MODE)
projectedNNsInfo <- function(My,Dy,SMy,PSMy,timesX,i,h) {
  w <- weightsRelativeToDistance(Dy[i,timesX+h])
  wPY <- list("i"=i,"Times_Projected"=timesX+h,
              "Dists" = Dy[i,timesX+h],
              "Weights" = as.vector(w),
              "Signatures" = SMy[timesX+h,],
              "Patterns" = PSMy[timesX+h],
              "Coordinates" = My[timesX+h,])
  return(wPY)
}

projectedNNsInfo_Lite <- function(My,Dy,SMy,PSMy,timesX,i,h) {
  w <- weightsRelativeToDistance(Dy)
  wPY <- list("i"=i,"Times_Projected"=timesX+h,
              "Dists" = Dy,
              "Weights" = as.vector(w),
              "Signatures" = SMy[timesX+h,],
              "Patterns" = PSMy[timesX+h],
              "Coordinates" = My[timesX+h,])
  return(wPY)
}
#=== Prerequisites
weightsRelativeToDistance <- function(distsVec) {
  #cat(paste("W:", distsVec, ".."))
  weights_1 = distsVec
  w.total = sum(weights_1)
  if (w.total==0) {
    w.total <- 0.0001
  }
  weights_2 = weights_1/w.total
  w <- exp(-weights_2)/sum(exp(-weights_2))
  return(w)
}

#= Prediction via Projected Neighborhood Topology
predictionY <- function(E,projNNy,zeroTolerance=(E+1)/2) {
  if (E >= 3) {
    predictedSignatureY <- rep(0,E-1)
    for (part in 1:length(predictedSignatureY)) {
      predictedSignatureY[part] <- ifelse(length(which(projNNy$Signatures[,part]==0))>zeroTolerance,
                                          0,
                                          sum(projNNy$Signatures[,part]*projNNy$Weights))
    }
  } else {
    predictedSignatureY <- ifelse(length(which(projNNy$Signatures==0))>zeroTolerance,
                                  0,
                                  sum(projNNy$Signatures*projNNy$Weights))
  }
  predictedPatternY <- patternVectorDifference(predictedSignatureY)
  return(data.frame(predictedSignatureY=predictedSignatureY,
                    predictedPatternY=predictedPatternY))
}

#=================================#
#= LAYER 3: CAUSALITY ASSESSMENT =#
#=================================#

#= Filling Pattern Causality Matrix
fillPCMatrix <- function(weighted,predictedPatternY,realPatternY,predictedSignatureY,realSignatureY,patternX,signatureX) {
  if (!anyNA(c(predictedPatternY,realPatternY,patternX))) {
    if (length(predictedPatternY)>0) {
      if (length(patternX)>0) {
        if (predictedPatternY==realPatternY) { # IF PREDICTION IS ACCURATE PROCCEED
          predictedCausalityStrength <- ifelse(weighted,erf(norm_vec(predictedSignatureY)/norm_vec(signatureX)),1)
          realCausalityStrength <- ifelse(weighted,erf(norm_vec(realSignatureY)/norm_vec(signatureX)),1)
          #predictedCausalityStrength <- ifelse(predictedCausalityStrength==0,1,predictedCausalityStrength)
          #predictedCausalityStrength <- ifelse(is.nan(predictedCausalityStrength),1,predictedCausalityStrength)
          #realCausalityStrength <- ifelse(realCausalityStrength==0,1,realCausalityStrength)
          #realCausalityStrength <- ifelse(is.nan(realCausalityStrength),1,realCausalityStrength)
          #= THIS IS THE LATEST STANDARD AS OF 27 JUNE 2020
          #predictedCausalityStrength <- 1
          #realCausalityStrength <- 1
        } else {
          predictedCausalityStrength <- 0
          realCausalityStrength <- 0
        }
      } else {
        stop("The length of the causal pattern of X is ZERO")
      }
    } else {
      stop("The length of the predicted pattern of Y is ZERO")
    }
  } else {
    predictedCausalityStrength <- NA
    realCausalityStrength <- NA
  }
  return(data.frame(real=realCausalityStrength,
                    predicted=predictedCausalityStrength))
}
#=== Prerequisites
norm_vec <- function(x) sqrt(sum(x^2))
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

#= Nature of Causality
natureOfCausality <- function(PC,dur,hashedpatterns,len) {
  # positiveCausality <- vector(mode = "double", length = length(X))
  # negativeCausality <- vector(mode = "double", length = length(X))
  # darkCausality <- vector(mode = "double", length = length(X))
  # noCausality <- vector(mode = "double", length = length(X))
  positiveCausality <- rep(NA, len)
  negativeCausality <- rep(NA, len)
  darkCausality <- rep(NA, len)
  noCausality <- rep(NA, len)
  for(i in dur) {
    cell <- which(!is.na(PC[,,i]), arr.ind = TRUE)
    #print(paste("FOR",i))
    if (!is.na(PC[cell[1],cell[2],i])) {
      #print("Inside IF 1")
      if (!is.nan(PC[cell[1],cell[2],i])) {
        #print("Inside IF 2")
        #if (PC[cell[1],cell[2],i]!=0) {
          if(!is.na(cell[1])) {
            #print("Inside IF 3")
            if(!is.na(cell[2])) {
              #print("Inside IF 4")
              #======================#
              #= POSITIVE CAUSALITY =#
              #======================#
              if(cell[1]==cell[2]) {
                if (cell[1]!=mean(1:length(hashedpatterns))) {
                  ### NO CAUSALITY CHECK
                  if (PC[cell[1],cell[2],i] == 0) {
                    noCausality[i] <- 1
                    positiveCausality[i] <- 0
                  } else {
                    positiveCausality[i] <- 1
                    noCausality[i] <- 0
                  }
                  negativeCausality[i] <- 0
                  darkCausality[i] <- 0
                  
                } else { # STO KENTRO TOY PC MATRIX
                  ### NO CAUSALITY CHECK
                  if (PC[cell[1],cell[2],i] == 0) {
                    noCausality[i] <- 1
                    darkCausality[i] <- 0 # PC[cell[1],cell[2],i]
                  } else {
                    noCausality[i] <- 0
                    darkCausality[i] <- 1 # PC[cell[1],cell[2],i]
                  }
                  negativeCausality[i] <- 0
                  positiveCausality[i] <- 0
                  
                }
              } 
              #======================#
              #= NEGATIVE CAUSALITY =#
              #======================#
              else if ((cell[1]+cell[2])==(length(hashedpatterns)+1)) {
                if (cell[1]!=mean(1:length(hashedpatterns))) {
                  ### NO CAUSALITY CHECK
                  if (PC[cell[1],cell[2],i] == 0) {
                    noCausality[i] <- 1
                    negativeCausality[i] <- 0 # PC[cell[1],cell[2],i]
                  } else {
                    noCausality[i] <- 0
                    negativeCausality[i] <- 1 # PC[cell[1],cell[2],i]
                  }
                  positiveCausality[i] <- 0
                  darkCausality[i] <- 0
                  
                } else {
                  ### NO CAUSALITY CHECK
                  if (PC[cell[1],cell[2],i] == 0) {
                    noCausality[i] <- 1
                    darkCausality[i] <- 0 # PC[cell[1],cell[2],i]
                  } else {
                    noCausality[i] <- 0
                    darkCausality[i] <- 1 # PC[cell[1],cell[2],i]
                  }
                  negativeCausality[i] <- 0
                  positiveCausality[i] <- 0
                  
                }
              } 
              #==================#
              #= DARK CAUSALITY =#
              #==================#
              else {
                ### NO CAUSALITY CHECK
                if (PC[cell[1],cell[2],i] == 0) {
                  noCausality[i] <- 1
                  darkCausality[i] <- 0 # PC[cell[1],cell[2],i]
                } else {
                  noCausality[i] <- 0
                  darkCausality[i] <- 1 # PC[cell[1],cell[2],i]
                }
                negativeCausality[i] <- 0
                positiveCausality[i] <- 0
                
              }
            }
          }
        #}
      }
    }
  }
  NOC <- data.frame(noCausality=noCausality,
                    Positive=positiveCausality,
                    Negative=negativeCausality,
                    Dark=darkCausality)
  return(NOC)
}

#===================#
#= FINAL FUNCTIONS =#
#===================#

#= "Full Details" Predicts directly from "i" to "h" steps ahead
PC.Mk.II.Full.Details <- function(X,Y,E,tau,metric,h,weighted) {
  ###################################
  ### STEP 0: PREPARATORY ACTIONS ###
  ###################################
  NNSPAN = E+1 # Former NN | Reserves a minimum number of nearest neighbors
  CCSPAN = (E-1)*tau # This will remove the common coordinate NNs
  hashedpatterns <- patternHashing(E)
  #####################################
  ### STEP 1: THE SHADOW ATTRACTORS ###
  #####################################
  #= [A] =# State Space
  Mx <- stateSpace(X,E,tau)
  My <- stateSpace(Y,E,tau)
  #= [B] =# Signature Space
  SMx <- signatureSpace(Mx,E)
  SMy <- signatureSpace(My,E)
  #= [C] =# Pattern Space
  PSMx <- patternSpace(SMx,E)
  PSMy <- patternSpace(SMy,E)
  #= [D] =# Distance Matrix | First row corresponds to t=1
  Dx <- distanceMatrix(Mx,metric)
  Dy <- distanceMatrix(My,metric)
  #= Check whether time series length is sufficient
  FCP <- firstCausalityPoint(E,tau,h,X)
  #= Calculate the main loop duration of the algorithm
  al_loop_dur <- FCP:(length(X)-(E-1)*tau-h)
  #= KEEPING THE PC MATRICES | Causality is considered only from FCP onwards
  predictedPCMatrix <- dataBank(type = "array",dimensions=c(3^(E-1),3^(E-1),length(Y)))
  realPCMatrix <- dataBank(type = "array",dimensions=c(3^(E-1),3^(E-1),length(Y)))
  #= KEEPING THE SIGNATURES
  predictedSignaturesY <- dataBank(type = "matrix",dimensions = c(length(Y),E-1))
  realSignaturesY <- dataBank(type = "matrix",dimensions = c(length(Y),E-1))
  causalSignaturesX <- dataBank(type = "matrix",dimensions = c(length(Y),E-1))
  #= KEEPING THE PATTERNS
  predictedPatternsY <- dataBank(type = "vector",dimensions = length(Y))
  realPatternsY <- dataBank(type = "vector",dimensions = length(Y))
  causalPatternsX <- dataBank(type = "vector",dimensions = length(Y))
  #= KEEPING THE CONVERTED SIGNATURES INTO ACTUAL VALUES
  predictedValuesY <- dataBank(type = "matrix",dimensions = c(length(Y),E))
  colnames(predictedValuesY) <- c("currVal",rep("predVal",E-1))
  realValuesY <- dataBank(type = "matrix",dimensions = c(length(Y),E))
  colnames(realValuesY) <- c("currVal",rep("futuVal",E-1))
  #= KEEPING NEIGHBORHOODS DETAILS
  NNsX <- rep(list(),length(Y)) 
  NNsYproj <- rep(list(),length(Y))  
  pb <- tkProgressBar(title = "Deploying PC Mk. II", min = 0,
                      max = length(al_loop_dur), width = 500)
  real_loop <- NA
  for(i in al_loop_dur) {
    if (!anyNA(c(Mx[i,],My[i+h,]))) {
      ###################################################################
      ### STEP 2: The Nearest Neighbours and their Future projections ###
      ###################################################################
      NNx <- pastNNsInfo(CCSPAN,NNSPAN,Mx,Dx,SMx,PSMx,i,h)
      if (!anyNA(NNx$dists)) {
        if (!anyNA(Dy[i,NNx$times+h])) {
          if (is.na(real_loop)) {
            real_loop <- i
          } else {
            real_loop <- c(real_loop,i)
          }
          projNNy <- projectedNNsInfo(My,Dy,SMy,PSMy,NNx$times,i,h)
          #= MEMORY LOG
          NNsX[[i]] <- NNx
          NNsYproj[[i]] <- projNNy
          #######################################################################
          ### STEP 3: The affected variable's predicted pattern h steps ahead ###
          #######################################################################
          predictedSignatureY <- predictionY(E,projNNy,zeroTolerance=E-1)$predictedSignatureY
          predictedPatternY <- predictionY(E,projNNy,zeroTolerance=E-1)$predictedPatternY[1]
          #= MEMORY LOG
          predictedSignaturesY[i,] <- predictedSignatureY
          predictedPatternsY[i] <- predictedPatternY
          #############################################
          ### STEP 4: The causal variable's pattern ###
          #############################################
          ####################signatureX <- signaE(E,SignX,i)
          signatureX <- SMx[i,]
          patternX <- PSMx[i,]
          #= MEMORY LOG
          causalSignaturesX[i,] <- signatureX
          causalPatternsX[i] <- patternX
          ####################################################
          ### STEP 5: The affected variable's real pattern ###
          ####################################################
          #######realSignatureY <- signaE(E,SignY,(i+h))
          realSignatureY <- SMy[(i+h),]
          realPatternY <- PSMy[i+h]
          #= MEMORY LOG
          realSignaturesY[i,] <- realSignatureY
          realPatternsY[i] <- realPatternY
          ##########################################################################
          ### STEP 6: The nature and intensity of causality at every time step t ###
          ##########################################################################
          pc <- fillPCMatrix(weighted,predictedPatternY,realPatternY,predictedSignatureY,realSignatureY,patternX,signatureX)
          predictedPCMatrix[which(hashedpatterns==patternX),which(hashedpatterns==predictedPatternY),i] <- pc$predicted
          realPCMatrix[which(hashedpatterns==patternX),which(hashedpatterns==predictedPatternY),i] <- pc$real
          ####################################################
          ### STEP * Convert Predicted Signature to Values ###
          ####################################################
          predictedValuesY[i,] <- convertSignatureToValue(E,tau,Y,i,h,predictedSignatureY)
          realValuesY[i,] <- Y[seq(from = i+h, to = i+h+(E-1)*tau, by = tau)]
        }
      }
    }
    setTkProgressBar(pb, i, label=paste( i/al_loop_dur[length(al_loop_dur)], 0),"% PC Mk. II Assignment Completion")
  }
  spectrumOfCausalityReal <- natureOfCausality(realPCMatrix,real_loop,hashedpatterns,X)
  spectrumOfCausalityPredicted <- natureOfCausality(predictedPCMatrix,real_loop,hashedpatterns,X)
  return(list(backtestTime=al_loop_dur,
              realValidTime=real_loop,
              spectrumOfCausalityReal=spectrumOfCausalityReal,
              spectrumOfCausalityPredicted=spectrumOfCausalityPredicted,
              Mx=Mx,My=My,NNsX=NNsX,NNsYproj=NNsYproj,
              predSigsY=predictedSignaturesY,predPattY=predictedPatternsY,
              causSigsX=causalSignaturesX,causPattX=causalPatternsX,
              realSigsY=realSignaturesY,realPattY=realPatternsY,
              predPC=predictedPCMatrix,realPC=realPCMatrix,
              predY=predictedValuesY,realY=realValuesY))
}

#= The one below is used to find the optimal pair of E and tau SEARCH
PC.Mk.II.Lightweight <- function(X,Y,E,tau,metric,h,weighted) {
  ###################################
  ### STEP 0: PREPARATORY ACTIONS ###
  ###################################
  NNSPAN = E+1 # Former NN | Reserves a minimum number of nearest neighbors
  CCSPAN = (E-1)*tau # This will remove the common coordinate NNs
  hashedpatterns <- patternHashing(E)
  #####################################
  ### STEP 1: THE SHADOW ATTRACTORS ###
  #####################################
  #= [A] =# State Space
  Mx <- stateSpace(X,E,tau)
  My <- stateSpace(Y,E,tau)
  #= [B] =# Signature Space
  SMx <- signatureSpace(Mx,E)
  SMy <- signatureSpace(My,E)
  #= [C] =# Pattern Space
  PSMx <- patternSpace(SMx,E)
  PSMy <- patternSpace(SMy,E)
  #= [D] =# Distance Matrix | First row corresponds to t=1
  Dx <- distanceMatrix(Mx,metric)
  Dy <- distanceMatrix(My,metric)
  #= Check whether time series length is sufficient
  FCP <- firstCausalityPoint(E,tau,h,X)
  #= Calculate the main loop duration of the algorithm
  al_loop_dur <- FCP:(length(X)-(E-1)*tau-h)
  #= Calculate the loop duration for out of sample forecasts
  out_of_sample_loop_dur <- ((length(X)-(E-1)*tau-h)+1):nrow(Mx)
  #= KEEPING THE PC MATRICES | Causality is considered only from FCP onwards
  predictedPCMatrix <- dataBank(type = "array",dimensions=c(3^(E-1),3^(E-1),length(Y)))
  #pb <- tkProgressBar(title = "Deploying PC Mk. II", min = 0,
  #                    max = length(al_loop_dur), width = 500)
  real_loop <- NA
  for(i in al_loop_dur) {
    if (!anyNA(c(Mx[i,],My[i+h,]))) {
      ###################################################################
      ### STEP 2: The Nearest Neighbours and their Future projections ###
      ###################################################################
      NNx <- pastNNsInfo(CCSPAN,NNSPAN,Mx,Dx,SMx,PSMx,i,h)
      if (!anyNA(NNx$dists)) {
        if (!anyNA(Dy[i,NNx$times+h])) {
          if (is.na(real_loop)) {
            real_loop <- i
          } else {
            real_loop <- c(real_loop,i)
          }
          projNNy <- projectedNNsInfo(My,Dy,SMy,PSMy,NNx$times,i,h)
          #######################################################################
          ### STEP 3: The affected variable's predicted pattern h steps ahead ###
          #######################################################################
          predictedSignatureY <- predictionY(E,projNNy,zeroTolerance=E-1)$predictedSignatureY
          predictedPatternY <- predictionY(E,projNNy,zeroTolerance=E-1)$predictedPatternY[1]
          #############################################
          ### STEP 4: The causal variable's pattern ###
          #############################################
          ####################signatureX <- signaE(E,SignX,i)
          signatureX <- SMx[i,]
          patternX <- PSMx[i,]
          ####################################################
          ### STEP 5: The affected variable's real pattern ###
          ####################################################
          #######realSignatureY <- signaE(E,SignY,(i+h))
          realSignatureY <- SMy[(i+h),]
          realPatternY <- PSMy[i+h]
          ##########################################################################
          ### STEP 6: The nature and intensity of causality at every time step t ###
          ##########################################################################
          pc <- fillPCMatrix(weighted,predictedPatternY,realPatternY,predictedSignatureY,realSignatureY,patternX,signatureX)
          predictedPCMatrix[which(hashedpatterns==patternX),which(hashedpatterns==predictedPatternY),i] <- pc$predicted
        }
      }
    }
    #setTkProgressBar(pb, i, label=paste( i/al_loop_dur[length(al_loop_dur)], 0),"% PC Mk. II In-Sample Assignment Completion")
  }
  causality <- natureOfCausality(predictedPCMatrix,real_loop,hashedpatterns,X)
  totalCausPercent <- 1-mean(causality$noCausality,na.rm = T)
  posiCausPercent <- mean(ifelse(causality$noCausality[real_loop]!=1,causality$Positive[real_loop],NA),na.rm = T)
  negaCausPercent <- mean(ifelse(causality$noCausality[real_loop]!=1,causality$Negative[real_loop],NA),na.rm = T)
  darkCausPercent <- mean(ifelse(causality$noCausality[real_loop]!=1,causality$Dark[real_loop],NA),na.rm = T)
  #return(list(causality,totalCausPercent,posiCausPercent,negaCausPercent,darkCausPercent))
  return(data.frame(total=totalCausPercent,positive=posiCausPercent,negative=negaCausPercent,dark=darkCausPercent))
}

#= The one below is used after we determine E and tau in order to conduct network analytics
PC.Mk.II.For.Network <- function(X,Y,E,tau,metric,h,weighted) {
  ###################################
  ### STEP 0: PREPARATORY ACTIONS ###
  ###################################
  NNSPAN = E+1 # Former NN | Reserves a minimum number of nearest neighbors
  CCSPAN = (E-1)*tau # This will remove the common coordinate NNs
  hashedpatterns <- patternHashing(E)
  #####################################
  ### STEP 1: THE SHADOW ATTRACTORS ###
  #####################################
  #= [A] =# State Space
  Mx <- stateSpace(X,E,tau)
  My <- stateSpace(Y,E,tau)
  #= [B] =# Signature Space
  SMx <- signatureSpace(Mx,E)
  SMy <- signatureSpace(My,E)
  #= [C] =# Pattern Space
  PSMx <- patternSpace(SMx,E)
  PSMy <- patternSpace(SMy,E)
  #= [D] =# Distance Matrix | First row corresponds to t=1
  Dx <- distanceMatrix(Mx,metric)
  Dy <- distanceMatrix(My,metric)
  #= Check whether time series length is sufficient
  FCP <- firstCausalityPoint(E,tau,h,X)
  #= Calculate the main loop duration of the algorithm
  al_loop_dur <- FCP:(length(X)-(E-1)*tau-h)
  #= Calculate the loop duration for out of sample forecasts
  out_of_sample_loop_dur <- ((length(X)-(E-1)*tau-h)+1):nrow(Mx)
  #= KEEPING THE PC MATRICES | Causality is considered only from FCP onwards
  predictedPCMatrix <- dataBank(type = "array",dimensions=c(3^(E-1),3^(E-1),length(Y)))
  #pb <- tkProgressBar(title = "Deploying PC Mk. II", min = 0,
  #                    max = length(al_loop_dur), width = 500)
  real_loop <- NA
  for(i in al_loop_dur) {
    if (!anyNA(c(Mx[i,],My[i+h,]))) {
      ###################################################################
      ### STEP 2: The Nearest Neighbours and their Future projections ###
      ###################################################################
      NNx <- pastNNsInfo(CCSPAN,NNSPAN,Mx,Dx,SMx,PSMx,i,h)
      if (!anyNA(NNx$dists)) {
        if (!anyNA(Dy[i,NNx$times+h])) {
          if (is.na(real_loop)) {
            real_loop <- i
          } else {
            real_loop <- c(real_loop,i)
          }
          projNNy <- projectedNNsInfo(My,Dy,SMy,PSMy,NNx$times,i,h)
          #######################################################################
          ### STEP 3: The affected variable's predicted pattern h steps ahead ###
          #######################################################################
          predictedSignatureY <- predictionY(E,projNNy,zeroTolerance=E-1)$predictedSignatureY
          predictedPatternY <- predictionY(E,projNNy,zeroTolerance=E-1)$predictedPatternY[1]
          #############################################
          ### STEP 4: The causal variable's pattern ###
          #############################################
          ####################signatureX <- signaE(E,SignX,i)
          signatureX <- SMx[i,]
          patternX <- PSMx[i,]
          ####################################################
          ### STEP 5: The affected variable's real pattern ###
          ####################################################
          #######realSignatureY <- signaE(E,SignY,(i+h))
          realSignatureY <- SMy[(i+h),]
          realPatternY <- PSMy[i+h]
          ##########################################################################
          ### STEP 6: The nature and intensity of causality at every time step t ###
          ##########################################################################
          pc <- fillPCMatrix(weighted,predictedPatternY,realPatternY,predictedSignatureY,realSignatureY,patternX,signatureX)
          predictedPCMatrix[which(hashedpatterns==patternX),which(hashedpatterns==predictedPatternY),i] <- pc$predicted
        }
      }
    }
    #setTkProgressBar(pb, i, label=paste( i/al_loop_dur[length(al_loop_dur)], 0),"% PC Mk. II In-Sample Assignment Completion")
  }
  causality <- natureOfCausality(predictedPCMatrix,real_loop,hashedpatterns,X)
  return(causality)
}

#= The one below uses DISTANCE VECTOR INSTEAD OF MATRIX
PC.Mk.II.For.Network.Lite <- function(X,Y,E,tau,metric,h,weighted,comCoordRemoval) {
  ###################################
  ### STEP 0: PREPARATORY ACTIONS ###
  ###################################
  NNSPAN = E+1 # Former NN | Reserves a minimum number of nearest neighbors
  #CCSPAN = E-1 # (E-1)*tau # This will remove the common coordinate NNs
  CCSPAN = ifelse(comCoordRemoval, (E-1)*tau, E-1)
  hashedpatterns <- patternHashing(E)
  #####################################
  ### STEP 1: THE SHADOW ATTRACTORS ###
  #####################################
  #= [A] =# State Space
  Mx <- stateSpace(X,E,tau)
  My <- stateSpace(Y,E,tau)
  #= [B] =# Signature Space
  SMx <- signatureSpace(Mx,E)
  SMy <- signatureSpace(My,E)
  #= [C] =# Pattern Space
  PSMx <- patternSpace(SMx,E)
  PSMy <- patternSpace(SMy,E)
  #= Check whether time series length is sufficient
  #FCP <- firstCausalityPoint(E,tau,h,X)
  #= Calculate the main loop duration of the algorithm
  #al_loop_dur <- (NNSPAN+1):nrow(Mx)
  al_loop_dur <- (NNSPAN+CCSPAN):nrow(Mx)
  #al_loop_dur <- FCP:(length(X)-(E-1)*tau-h)
  #= Calculate the loop duration for out of sample forecasts
  #out_of_sample_loop_dur <- ((length(X)-(E-1)*tau-h)+1):nrow(Mx)
  #= KEEPING THE PC MATRICES | Causality is considered only from FCP onwards
  predictedPCMatrix <- dataBank(type = "array",dimensions=c(3^(E-1),3^(E-1),nrow(Mx)))
  #predictedPCMatrix <- dataBank(type = "array",dimensions=c(3^(E-1),3^(E-1),length(Y)))
  #pb <- tkProgressBar(title = "Deploying PC Mk. II", min = 0,
  #                    max = length(al_loop_dur), width = 500)
  real_loop <- NA
  for(i in al_loop_dur) {
    if (!anyNA(c(Mx[i,], My[i+h,]))) {
      ###################################################################
      ### STEP 2: The Nearest Neighbours and their Future projections ###
      ###################################################################
      Dx <- distanceVector(point = Mx[i,], candidateNNs = Mx[1:(i-CCSPAN-h),],n = metric)
      names(Dx) <- 1:(i-CCSPAN-h)
      if (!anyNA(Dx[order(Dx)][1:NNSPAN])) {
        NNx <- pastNNsInfo_Lite(CCSPAN,NNSPAN,Mx,Dx,SMx,PSMx,i,h)
        if (!anyNA(NNx$dists)) {
            Dy <- distanceVector(point = My[i,], candidateNNs = My[NNx$times+h,],n = metric)
            names(Dy) <- NNx$times+h
            if (!anyNA(Dy)) {
              if (is.na(real_loop)) {
                real_loop <- i
              } else {
                real_loop <- c(real_loop,i)
              }
              projNNy <- projectedNNsInfo_Lite(My,Dy,SMy,PSMy,NNx$times,i,h)
              #######################################################################
              ### STEP 3: The affected variable's predicted pattern h steps ahead ###
              #######################################################################
              predictedSignatureY <- predictionY(E,projNNy,zeroTolerance=E-1)$predictedSignatureY
              predictedPatternY <- predictionY(E,projNNy,zeroTolerance=E-1)$predictedPatternY[1]
              #############################################
              ### STEP 4: The causal variable's pattern ###
              #############################################
              ####################signatureX <- signaE(E,SignX,i)
              signatureX <- SMx[i,]
              patternX <- PSMx[i,]
              ####################################################
              ### STEP 5: The affected variable's real pattern ###
              ####################################################
              #######realSignatureY <- signaE(E,SignY,(i+h))
              realSignatureY <- SMy[(i+h),]
              realPatternY <- PSMy[i+h]
              ##########################################################################
              ### STEP 6: The nature and intensity of causality at every time step t ###
              ##########################################################################
              pc <- fillPCMatrix(weighted,predictedPatternY,realPatternY,predictedSignatureY,realSignatureY,patternX,signatureX)
              predictedPCMatrix[which(hashedpatterns==patternX),which(hashedpatterns==predictedPatternY),i] <- pc$predicted
            }
        }

      }
    }
    #setTkProgressBar(pb, i, label=paste( i/al_loop_dur[length(al_loop_dur)], 0),"% PC Mk. II In-Sample Assignment Completion")
  }
  causality <- natureOfCausality(predictedPCMatrix,real_loop,hashedpatterns,nrow(Mx))
  totalCausPercent <- 1-mean(causality$noCausality,na.rm = T)
  totalCausPercent <- ifelse(is.nan(totalCausPercent),NA,totalCausPercent)
  posiCausPercent <- mean(ifelse(causality$noCausality[real_loop]!=1,
                                 causality$Positive[real_loop],NA),na.rm = T)
  posiCausPercent <- ifelse(is.nan(posiCausPercent),NA,posiCausPercent)
  negaCausPercent <- mean(ifelse(causality$noCausality[real_loop]!=1,
                                 causality$Negative[real_loop],NA),na.rm = T)
  negaCausPercent <- ifelse(is.nan(negaCausPercent),NA,negaCausPercent)
  darkCausPercent <- mean(ifelse(causality$noCausality[real_loop]!=1,
                                 causality$Dark[real_loop],NA),na.rm = T)
  darkCausPercent <- ifelse(is.nan(darkCausPercent),NA,darkCausPercent)
  #return(list(causality,totalCausPercent,posiCausPercent,negaCausPercent,darkCausPercent))
  return(list(causality=causality,
              nature=data.frame(total=totalCausPercent,
                                positive=posiCausPercent,
                                negative=negaCausPercent,
                                dark=darkCausPercent)))
}

#= "Into the Dark" predicts step by step based on previous predictions
PC.Mk.II.into.the.Dark <- function(X,Y,E,tau,metric,spot,darkHorizon) {
  NNSPAN = E+1 # Former NN | Reserves a minimum number of nearest neighbors
  CCSPAN = (E-1)*tau # This will remove the common coordinate NNs
  hashedpatterns <- patternHashing(E)
  #= [A] =# State Space
  Mx <- stateSpace(X,E,tau)
  My <- stateSpace(Y,E,tau)
  #= [B] =# Signature Space
  SMx <- signatureSpace(Mx,E)
  SMy <- signatureSpace(My,E)
  #= [C] =# Pattern Space
  PSMx <- patternSpace(SMx,E)
  PSMy <- patternSpace(SMy,E)
  #= [D] =# Distance Matrix | First row corresponds to t=1
  Dx <- distanceMatrix(Mx,metric)
  Dy <- distanceMatrix(My,metric)
  predictedValuesY <- dataBank(type = "matrix",dimensions = c(darkHorizon+1,E))
  #= First prediction to disentagle ourselves from Y
  if (!anyNA(c(Mx[spot,]))) {
    NNx <- pastNNsInfo(CCSPAN,NNSPAN,Mx,Dx,SMx,PSMx,spot,h=0)
    if (!anyNA(Dy[spot,NNx$times])) {
      projNNy <- projectedNNsInfo(My,Dy,SMy,PSMy,NNx$times,spot,h=0)
      predictedSignatureY <- predictionY(E,projNNy,zeroTolerance=E-1)$predictedSignatureY
      predictedValuesY[1,] <- convertSignatureToValue(E,tau,Y,spot,h=0,predictedSignatureY)
    }
  }
  #= STSRTING NEIGHBORHOOD IN X
  j <- 2
  for(h in 1:darkHorizon) {
    if (!anyNA(c(Mx[spot,]))) {
      NNx <- pastNNsInfo(CCSPAN,NNSPAN,Mx,Dx,SMx,PSMx,spot,h)
      if (!anyNA(NNx$dists)) {
        if (!anyNA(Dy[spot,NNx$times+h])) {
          projNNy <- projectedNNsInfo(My,Dy,SMy,PSMy,NNx$times,spot,h)
          predictedSignatureY <- predictionY(E,projNNy,zeroTolerance=E-1)$predictedSignatureY
          predictedValuesY[j,] <- convertSignatureToValueOutOfSample(E,tau,predictedValuesY[j-1,E],spot,h,predictedSignatureY)
        }
      }
    }
  }
  return(predY=predictedValuesY)
}

#=================================================#
#= PATTERN CAUSALITY DYNAMIC NETWORK CALCULATION =#
#=================================================#

pcAccuracy <- function(dataset,E,tau,metric,h,weighted,comCoordRemoval) {
  dataset <- as.matrix(dataset)
  # STORAGE ARRAYS
  couplingsTotal <- dataBank(type = "matrix",dimensions=c(ncol(dataset),ncol(dataset)))
  couplingsPosi <- dataBank(type = "matrix",dimensions=c(ncol(dataset),ncol(dataset)))
  couplingsNega <- dataBank(type = "matrix",dimensions=c(ncol(dataset),ncol(dataset)))
  couplingsDark <- dataBank(type = "matrix",dimensions=c(ncol(dataset),ncol(dataset)))
  # DEPLOY PC MK. II LIGHT Version
  #pb <- tkProgressBar(title = "Still Running on Coal :P", min = 0,
  #                    max = ncol(dataset), width = 300)
  for (i in 1:ncol(dataset)) {
    for (j in 1:ncol(dataset)) {
      if (i != j) {
        if (firstCausalityPointCHECK(E,tau,h,dataset[,i])) {
          if (firstCausalityPointCHECK(E,tau,h,dataset[,j])) {
            temp <- PC.Mk.II.For.Network.Lite(dataset[,i],dataset[,j],
                                              E,tau,metric,h,weighted,comCoordRemoval)
            #temp <- PC.Mk.II.Lightweight(dataset[,i],dataset[,j],E,tau,metric,h,weighted)
            couplingsTotal[i,j] <- temp$nature$total
            couplingsPosi[i,j] <- temp$nature$positive
            couplingsNega[i,j] <- temp$nature$negative
            couplingsDark[i,j] <- temp$nature$dark
          }
        }
      }
    }
    #setTkProgressBar(pb, i, label=paste( i/ncol(dataset)*100, 0),
    #                 "% towards Arc Reactor")
  }
  return(data.frame(E=E,tau=tau,total=mean(couplingsTotal, na.rm = T),
                    positive=mean(couplingsPosi, na.rm = T),
                    negative=mean(couplingsNega, na.rm = T),
                    dark=mean(couplingsDark, na.rm = T)))
}
patternCausalityDynamicNetwork <- function(dataset,E,tau,metric,h,weighted,comCoordRemoval) {
  # DURATION OF EXPERIMENT
  #FCP <- firstCausalityPoint(E,tau,h,dataset[,1])
  #dur <- FCP:(nrow(dataset)-(E-1)*tau-h)
  # STORAGE ARRAYS
  couplingsPosi <- dataBank(type = "array",dimensions=c(ncol(dataset),ncol(dataset),nrow(dataset)-tau))
  couplingsNega <- dataBank(type = "array",dimensions=c(ncol(dataset),ncol(dataset),nrow(dataset)-tau))
  couplingsDark <- dataBank(type = "array",dimensions=c(ncol(dataset),ncol(dataset),nrow(dataset)-tau))
  # DEPLOY PC MK. II LIGHT Version
  start.time <- Sys.time()
  pb <- tkProgressBar(title = "Still Running on Coal :P", min = 0,
                      max = ncol(dataset), width = 300)
  for (i in 1:ncol(dataset)) {
    print(paste("CAUSE: ", colnames(dataset)[i]))
    for (j in 1:ncol(dataset)) {
      print(paste("EFFECT: ", colnames(dataset)[j]))
      if (i != j) {
        if (firstCausalityPointCHECK(E,tau,h,dataset[,i])) {
          if (firstCausalityPointCHECK(E,tau,h,dataset[,j])) {
            temp <- PC.Mk.II.For.Network.Lite(dataset[,i],dataset[,j],
                                              E,tau,metric=metric,h,weighted,comCoordRemoval)
            #temp <- PC.Mk.II.Lightweight(dataset[,i],dataset[,j],
            #                                  E,tau,metric="euclidean",h,weighted)
            #print(paste("temp$Positive: ", length(temp$Positive)))
            #print(paste("nrow dataset: ", nrow(dataset)))
            couplingsPosi[i,j,] <- temp$causality$Positive
            couplingsNega[i,j,] <- temp$causality$Negative
            couplingsDark[i,j,] <- temp$causality$Dark
          }
        }
      }
    }
    setTkProgressBar(pb, i, label=paste( i/ncol(dataset)*100, 0),
                     "% towards Arc Reactor")
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste("Calculation duration: ", time.taken))
  return(list(positive=couplingsPosi,
              negative=couplingsNega,
              dark=couplingsDark,
              items=colnames(dataset)))
}
causalNetworkIgraph <- function(complexNetMatrix,type,validTime,isWeighted) {
  dynamicNetwork <- whichNetworkAspect(complexNetMatrix,type)
  dur <- validTime
  iNetworks <- list()
  i <- 1
  for (t in validTime) {
    g <- graph.adjacency(dynamicNetwork[,,t], 
                         mode = "directed", weighted = isWeighted, diag = FALSE,
                         add.colnames = NULL, add.rownames = NA)
    V(g)$names <- complexNetMatrix$items
    V(g)$IDs <- 1:length(complexNetMatrix$items)
    iNetworks[[i]] <- g
    i <- i + 1
  }
  return(iNetworks)
}

#=======================#
#= AUXILIARY FUNCTIONS =#
#=======================#

zero_counter <- function(vec) {
  return(length(which(vec==0)))
}
zero_filtering <- function(vec,threshold) {
  return(zero_counter(vec)<threshold)
}
na_counter <- function(vec) {
  return(length(which(is.na(vec))))
}
na_filtering <- function(vec,threshold) {
  return(na_counter(vec)<threshold)
}

optimalParametersSearch <- function(Emax,tauMax,metric,dataset,comCoordRemoval) {
  E_array <- 2:Emax
  tau_array <- 1:tauMax
  testsTotal <- matrix(data = NA, nrow = length(E_array), ncol = length(tau_array))
  testsPosi <- matrix(data = NA, nrow = length(E_array), ncol = length(tau_array))
  testsNega <- matrix(data = NA, nrow = length(E_array), ncol = length(tau_array))
  testsDark <- matrix(data = NA, nrow = length(E_array), ncol = length(tau_array))
  start.time <- Sys.time()
  pb <- tkProgressBar(title = "Still Running on Coal :P", min = 0,
                      max = max(E_array), width = 300)
  for (E in E_array) {
    print(paste("Testing | E: ",E))
    for (tau in tau_array) {
      print(paste("Testing | tau: ",tau))
      temp <- pcAccuracy(dataset = dataset, 
                         E=E, tau=tau, metric = metric, h=0, FALSE,comCoordRemoval)
      testsTotal[E-1,tau] <- temp$total
      testsPosi[E-1,tau] <- temp$positive
      testsNega[E-1,tau] <- temp$negative
      testsDark[E-1,tau] <- temp$dark
    }
    setTkProgressBar(pb, E, label=paste( E/max(E_array)*100, 0),
                     "% towards Arc Reactor")
  }
  #= MANIPULATING THE RESULTS
  accuracyPerE <- list()
  for (E in 2:Emax) {
    accuracyPerE[[E-1]] <- cbind(t(testsTotal)[,E-1],
                                 t(testsPosi)[,E-1],
                                 t(testsNega)[,E-1],
                                 t(testsDark)[,E-1])
    colnames(accuracyPerE[[E-1]]) <- c("Total","of which Positive","of which Negative","of which Dark")
    rowLABELS <- vector()
    for (tau in 1:tauMax) {
      rowLABELS <- c(rowLABELS, c(paste("E =",E,"tau =",tau)))
    }
    rownames(accuracyPerE[[E-1]]) <- rowLABELS
  }
  #= ACCURACY SUMMARY
  accuracySummary <- accuracyPerE[[1]]
  for (i in 2:length(accuracyPerE)) {
    accuracySummary <- rbind(accuracySummary, accuracyPerE[[i]])
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste("Calculation duration: ", time.taken))
  return(accuracySummary)
}
averageCausalityAspectsPerT <- function(dataset, network) {
  maxPossibleCausality <- vector(length = nrow(dataset))
  for (i in 1:nrow(dataset_RTA)) {
    n <- length(which(!is.na(dataset[i,])))
    #print(paste(i,"NON-NA VARS", n))
    maxPossibleCausality[i] <- n^2 - n
  }
  #maxPossibleCausality <- ncol(dataset_RTA)^2 - ncol(dataset_RTA)
  aggregatePosiPC_perYear <- vector(mode = "double",length = dim(network$positive)[3])
  aggregateNegaPC_perYear <- vector(mode = "double",length = dim(network$positive)[3])
  aggregateDarkPC_perYear <- vector(mode = "double",length = dim(network$positive)[3])
  for (i in 1:dim(network$positive)[3]) {
    aggregatePosiPC_perYear[i] <- sum(network$positive[,,i], na.rm = T)
    aggregateNegaPC_perYear[i] <- sum(network$negative[,,i], na.rm = T)
    aggregateDarkPC_perYear[i] <- sum(network$dark[,,i], na.rm = T)
  }
  normAggrPosi_perYear <- aggregatePosiPC_perYear / tail(maxPossibleCausality,length(aggregatePosiPC_perYear))
  normAggrNega_perYear <- aggregateNegaPC_perYear / tail(maxPossibleCausality,length(aggregateNegaPC_perYear))
  normAggrDark_perYear <- aggregateDarkPC_perYear / tail(maxPossibleCausality,length(aggregateDarkPC_perYear))
  return(data.frame(positive=normAggrPosi_perYear,negative=normAggrNega_perYear,dark=normAggrDark_perYear))
}

graphPCTimeSeries <- function(dataset,colorPalette,surroundTheme,titles) {
  g <- ggplot(dataset, aes(x = time)) + 
    geom_line(aes(y=positive, colour="Positive"),size=1) +
    geom_line(aes(y=negative, colour="Negative"),size=1) +
    geom_line(aes(y=dark, colour="Dark"),size=1) +
    # ANIMATION PARAMETERS
    #coord_cartesian(clip = 'off') +
    scale_colour_manual(name=NULL,values=colorPalette,
                        guide = guide_legend(reverse=TRUE))+
    labs(title = titles[1],
         x = titles[2],
         y = titles[3]) +
    theme_minimal() + 
    theme(panel.grid = element_blank()) +
    #theme(legend.position="bottom", legend.box = "horizontal") +
    theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5)) +
    surroundTheme +
    theme(axis.title.x = element_text(
      margin = margin(t = 15, b = 15)
    ),
    axis.title.y = element_text( angle = 90,
                                 margin = margin(r = 15, l = 15)
    ))
  return(g)
}

ggplotThemeDesigner <- function(titleSize,axesTitleSize,
                                axesTextSizeX,axesTextSizeY,
                                axesTicksLength,legElemTextSize,
                                legX,legY) {
  customTheme <- theme(plot.title = element_text(family="Times", 
                                                 face="bold", colour="black",
                                                 size=titleSize, hjust = 0.5),
                       axis.title.x = element_text(size = axesTitleSize, family="Times",
                                                   colour = "black", face = "bold"),
                       axis.title.y = element_text(size = axesTitleSize, family="Times",
                                                   colour = "black", face = "bold"),
                       axis.text.x = element_text(size = axesTextSizeX, family="Times",
                                                  margin = margin(t = .5, unit = "cm"),
                                                  colour = "black"),
                       axis.text.y = element_text(size = axesTextSizeY, family="Times",
                                                  margin = margin(r = .5, unit = "cm"),
                                                  colour = "black"),
                       panel.grid = element_blank(),
                       legend.position = c(legX, legY),
                       legend.direction = "vertical",
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       axis.line = element_line(colour = "black"),
                       axis.ticks.length= unit(-axesTicksLength, "cm"),
                       legend.title = element_text(size = legElemTextSize, family="Times"),
                       legend.text = element_text(size = legElemTextSize, family="Times"),
                       legend.key = element_rect(fill = "white"),
                       legend.key.size = unit(2,"line"),
                       legend.box.background = element_rect(size=1),
                       legend.box.margin = margin(0, 0, 0, 0)) 
  return(customTheme)
}

#=====================================================#
#= FUNCTIONS FOR CUSTOM-A-PRIORI DECIDED CAUSALITIES =#
#=====================================================#

#dataset=datasetRTA[[1]]
#E=2
#tau=1
#metric=2
#h=0
#weighted=FALSE
#comCoordRemoval=TRUE
#causes=1:2
#effects=3:4
#cardinality=4

pcAccuracy_DistanceVectors <- function(dataset,E,tau,metric,h,
                                       weighted,comCoordRemoval,
                                       causes,effects,cardinality) {
  dataset <- as.matrix(dataset)
  # STORAGE ARRAYS
  couplingsTotal <- dataBank(type = "matrix",dimensions=c(ncol(dataset),ncol(dataset)))
  couplingsPosi <- dataBank(type = "matrix",dimensions=c(ncol(dataset),ncol(dataset)))
  couplingsNega <- dataBank(type = "matrix",dimensions=c(ncol(dataset),ncol(dataset)))
  couplingsDark <- dataBank(type = "matrix",dimensions=c(ncol(dataset),ncol(dataset)))
  # DEPLOY PC MK. II LIGHT Version
  #pb <- tkProgressBar(title = "Still Running on Coal :P", min = 0,
  #                    max = ncol(dataset), width = 300)
  for (i in causes) {
    for (j in effects) {
      if (i != j) {
        if (firstCausalityPointCHECK(E,tau,h,dataset[,i])) {
          if (firstCausalityPointCHECK(E,tau,h,dataset[,j])) {
            #print(paste("Cause, i:",i,"Effect, j:",j))
            temp <- PC.Mk.II.For.Network.Lite(dataset[,i],dataset[,j],
                                              E,tau,metric,h,weighted,comCoordRemoval)
            #temp <- PC.Mk.II.Lightweight(dataset[,i],dataset[,j],E,tau,metric,h,weighted)
            couplingsTotal[i,j] <- temp$nature$total
            couplingsPosi[i,j] <- temp$nature$positive
            couplingsNega[i,j] <- temp$nature$negative
            couplingsDark[i,j] <- temp$nature$dark
          }
        }
      }
    }
    #setTkProgressBar(pb, i, label=paste( i/ncol(dataset)*100, 0),
    #                 "% towards Arc Reactor")
  }
  positive=sum(couplingsPosi, na.rm = T)/cardinality
  negative=sum(couplingsNega, na.rm = T)/cardinality
  dark=sum(couplingsDark, na.rm = T)/cardinality
  total=sum(couplingsTotal, na.rm = T)/cardinality #sum(positive,negative,dark)
  return(data.frame(E=E,tau=tau,total=total,positive=positive,negative=negative,dark=dark))
}

#pcAccuracy_CustomStructure(dataset,E,tau,metric,h,
#                                       weighted,comCoordRemoval,
#                                       causes,effects,cardinality)


optimalParametersSearch_DistanceVectors <- function(Emax,tauMax,metric,dataset,comCoordRemoval,
                                                    causes,effects,cardinality) {
  E_array <- 2:Emax
  tau_array <- 1:tauMax
  testsTotal <- matrix(data = NA, nrow = length(E_array), ncol = length(tau_array))
  testsPosi <- matrix(data = NA, nrow = length(E_array), ncol = length(tau_array))
  testsNega <- matrix(data = NA, nrow = length(E_array), ncol = length(tau_array))
  testsDark <- matrix(data = NA, nrow = length(E_array), ncol = length(tau_array))
  #start.time <- Sys.time()
  #pb <- tkProgressBar(title = "Still Running on Coal :P", min = 0,
  #                    max = max(E_array), width = 300)
  for (E in E_array) {
    #print(paste("Testing | E: ",E))
    for (tau in tau_array) {
      #print(paste("Testing | tau: ",tau))
      temp <- pcAccuracy_CustomStructure(dataset = dataset, 
                         E=E, tau=tau, metric = metric, h=0, FALSE,comCoordRemoval,
                         causes,effects,cardinality)
      testsTotal[E-1,tau] <- temp$total
      testsPosi[E-1,tau] <- temp$positive
      testsNega[E-1,tau] <- temp$negative
      testsDark[E-1,tau] <- temp$dark
    }
    #setTkProgressBar(pb, E, label=paste( E/max(E_array)*100, 0),
    #                 "% towards Arc Reactor")
  }
  #= MANIPULATING THE RESULTS
  accuracyPerE <- list()
  for (E in 2:Emax) {
    accuracyPerE[[E-1]] <- cbind(t(testsTotal)[,E-1],
                                 t(testsPosi)[,E-1],
                                 t(testsNega)[,E-1],
                                 t(testsDark)[,E-1])
    colnames(accuracyPerE[[E-1]]) <- c("Total","of which Positive","of which Negative","of which Dark")
    rowLABELS <- vector()
    for (tau in 1:tauMax) {
      rowLABELS <- c(rowLABELS, c(paste("E =",E,"tau =",tau)))
    }
    rownames(accuracyPerE[[E-1]]) <- rowLABELS
  }
  #= ACCURACY SUMMARY
  accuracySummary <- accuracyPerE[[1]]
  for (i in 2:length(accuracyPerE)) {
    accuracySummary <- rbind(accuracySummary, accuracyPerE[[i]])
  }
  #end.time <- Sys.time()
  #time.taken <- end.time - start.time
  #print(paste("Calculation duration: ", time.taken))
  return(as.data.frame(accuracySummary))
}

#datasetLIST=datasetRTA
#Emax=3
#tauMax=5
#metric=2
#comCoordRemoval=TRUE
#causes=1:2
#effects=3:4
#cardinality=4
#dataset=datasetRTA[[1]]

#optimalParametersSearch_CustomStructure(Emax,tauMax,metric,dataset,comCoordRemoval,
#                                                    causes,effects,cardinality)


multiDatasetOptimalParameterSearch_DistanceVectors <- function(Emax,tauMax,
                                                               metric,datasetLIST,
                                                               comCoordRemoval,
                                                               causes,effects,
                                                               cardinality) {
  #= PREPARATION
  test <- optimalParametersSearch_CustomStructure(Emax,tauMax,
                                                  metric,datasetLIST[[1]],
                                                  comCoordRemoval,
                                                  causes,effects,
                                                  cardinality)
  EandTauperDatasetTotal <- matrix(data = NA, nrow = nrow(test), ncol = length(datasetLIST))
  EandTauperDatasetPosi <- matrix(data = NA, nrow = nrow(test), ncol = length(datasetLIST))
  EandTauperDatasetNega <- matrix(data = NA, nrow = nrow(test), ncol = length(datasetLIST))
  EandTauperDatasetDark <- matrix(data = NA, nrow = nrow(test), ncol = length(datasetLIST))
  rownames(EandTauperDatasetTotal) <- rownames(test)
  rownames(EandTauperDatasetPosi) <- rownames(test)
  rownames(EandTauperDatasetNega) <- rownames(test)
  rownames(EandTauperDatasetDark) <- rownames(test)
  colnames(EandTauperDatasetTotal) <- names(datasetLIST)
  colnames(EandTauperDatasetPosi) <- names(datasetLIST)
  colnames(EandTauperDatasetNega) <- names(datasetLIST)
  colnames(EandTauperDatasetDark) <- names(datasetLIST)
  #= RUN FOR ALL DATASETS
  start.time <- Sys.time()
  pb <- tkProgressBar(title = "Still Running on Coal :P", min = 0,
                      max = length(datasetLIST), width = 300)
  for (i in 1:length(datasetLIST)) {
    print(paste("Current County No",i,names(datasetLIST)[i]))
    test <- optimalParametersSearch_CustomStructure(Emax,tauMax,
                                                    metric,datasetLIST[[i]],
                                                    comCoordRemoval,
                                                    causes,effects,
                                                    cardinality)
    EandTauperDatasetTotal[,i] <- test$Total
    EandTauperDatasetPosi[,i] <- test$`of which Positive`
    EandTauperDatasetNega[,i] <- test$`of which Negative`
    EandTauperDatasetDark[,i] <-test$`of which Dark`
    setTkProgressBar(pb, i, label=paste( i/length(datasetLIST)*100, 0),
                     "% towards Arc Reactor")
    
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste("Calculation duration: ", time.taken))
  write.csv(EandTauperDatasetTotal,file = "D:/R_Programs/PC-Mk-II/data_output/outputCSV/EandTauperDatasetTotal.csv")
  write.csv(EandTauperDatasetPosi,file = "D:/R_Programs/PC-Mk-II/data_output/outputCSV/EandTauperDatasetOfWhichPosi.csv")
  write.csv(EandTauperDatasetNega,file = "D:/R_Programs/PC-Mk-II/data_output/outputCSV/EandTauperDatasetOfWhichNega.csv")
  write.csv(EandTauperDatasetDark,file = "D:/R_Programs/PC-Mk-II/data_output/outputCSV/EandTauperDatasetOfWhichDark.csv")
  return(list(EandTauperDatasetTotal,EandTauperDatasetPosi,EandTauperDatasetNega,EandTauperDatasetNega))
}


#dataset=datasetRTA[[1]]
#E=2
#tau=1
#metric=2
#h=0
#weighted=FALSE
#comCoordRemoval=TRUE
#causes=1:2
#effects=3:4

patternCausalityDynamicNetwork_DistanceVectors <- function(dataset,E,tau,metric,h,
                                       weighted,comCoordRemoval,causes,effects) {
  dataset <- as.matrix(dataset)
  # STORAGE ARRAYS
  couplingsTotal <- matrix(data = NA, nrow = ncol(dataset), ncol = ncol(dataset)) # dataBank(type = "matrix",dimensions=c(ncol(dataset),ncol(dataset)))
  couplingsPosi <- matrix(data = NA, nrow = ncol(dataset), ncol = ncol(dataset))
  couplingsNega <- matrix(data = NA, nrow = ncol(dataset), ncol = ncol(dataset))
  couplingsDark <- matrix(data = NA, nrow = ncol(dataset), ncol = ncol(dataset))
  # DEPLOY PC MK. II LIGHT Version
  pb <- tkProgressBar(title = "Still Running on Coal :P", min = 0,
                      max = ncol(dataset), width = 300)
  rownames(couplingsPosi) <- causes
  colnames(couplingsPosi) <- effects
  rownames(couplingsNega) <- causes
  colnames(couplingsNega) <- effects
  rownames(couplingsDark) <- causes
  colnames(couplingsDark) <- effects
  for (i in causes) {
    for (j in effects) {
      if (i != j) {
        if (firstCausalityPointCHECK(E,tau,h,dataset[,i])) {
          if (firstCausalityPointCHECK(E,tau,h,dataset[,j])) {
            print(paste("Cause, i:",i,"Effect, j:",j))
            temp <- PC.Mk.II.For.Network.Lite(dataset[,i],dataset[,j],
                                              E,tau,metric,h,weighted,comCoordRemoval)
            #temp <- PC.Mk.II.Lightweight(dataset[,i],dataset[,j],E,tau,metric,h,weighted)
            couplingsTotal[i,j] <- temp$nature$total
            couplingsPosi[i,j] <- temp$nature$positive * couplingsTotal[i,j]
            couplingsNega[i,j] <- temp$nature$negative * couplingsTotal[i,j]
            couplingsDark[i,j] <- temp$nature$dark * couplingsTotal[i,j]
            
            couplingsPosi[i,j] <- ifelse(is.nan(couplingsPosi[i,j]),NA,couplingsPosi[i,j])
            couplingsNega[i,j] <- ifelse(is.nan(couplingsNega[i,j]),NA,couplingsNega[i,j])
            couplingsDark[i,j] <- ifelse(is.nan(couplingsDark[i,j]),NA,couplingsDark[i,j])
          }
        }
      }
    }
    setTkProgressBar(pb, i, label=paste( i/ncol(dataset)*100, 0),
                     "% towards Arc Reactor")
  }
  deliverable <- data.frame(positive=couplingsPosi,negative=couplingsNega,dark=couplingsDark)
  #row.names(deliverable) <- c("PRCP","TEMP","CASES","DEATHS")
  return(deliverable)
}

multiDatasetPCNET_DistanceVectors <- function(E,tau,metric,h,datasetLIST,weighted,
                                              comCoordRemoval,causes,effects) {
  #= PREPARATION
  PC_Networks_List <- list()
  #= RUN FOR ALL DATASETS
  start.time <- Sys.time()
  pb <- tkProgressBar(title = "Still Running on Coal :P", min = 0,
                      max = length(datasetLIST), width = 300)
  for (i in 1:length(datasetLIST)) {
    print(paste("Current County No",i,names(datasetLIST)[i]))
    temp <- patternCausalityDynamicNetwork_CustomStructure(datasetLIST[[i]],E,tau,metric,h,
                                                           weighted,comCoordRemoval,causes,effects)
    PC_Networks_List[[i]] <- temp
    setTkProgressBar(pb, i, label=paste( i/length(datasetLIST)*100, 0),
                     "% towards Arc Reactor")
    
  }
  names(PC_Networks_List) <- names(datasetLIST)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste("Calculation duration: ", time.taken))
  return(PC_Networks_List)
}



