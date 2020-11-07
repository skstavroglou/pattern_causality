patternCausality <- function(X,Y,E,tau,metric="euclidean",h) {
  NsnUWondQNfJtTsPDYXw <- E
  if (NsnUWondQNfJtTsPDYXw==2) {
    ZyBoRefCszHaUHJatHza <- c(6,12,18)
  } else if (NsnUWondQNfJtTsPDYXw==3) {
    ZyBoRefCszHaUHJatHza <- c(30,36,42,54,60,66,78,84,90)
  }
  usSMNttjNIjdA5oIdrzD <- X 
  pJKoqYsmrWnGaouxlXfl <- Y
  gCE0RdhbXDuADvpHQEgF <- tau
  iRJdEuwumgRyL0ihPcuQ <- h
  yhAs0HbHiyZyIeejxKlc <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  iRJdEuwumgRyLOihPcuQ <- h
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    pJKoqYsmrWnGaouxiXfl <- Y
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    pJKoqYsmrWnGaouxiXfl <- Y
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  gCEORdhbXDuADvpHQEgF <- tau
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    usSMNttjNIjdASoIdrzD <- X 
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      gCEORdhbXDuADvpHQEgF <- tau
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  yhAsOHbHiyZyIeejxKlc <- matrix(NA, length(usSMNttjNIjdASoIdrzD)-NsnUWondQNfJtTsPDYXw+1-gCEORdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(BylVf9ZwATNwlZKTFBny in 1:nrow(yhAsOHbHiyZyIeejxKlc)) {
    yhAsOHbHiyZyIeejxKlc[BylVf9ZwATNwlZKTFBny,] <- usSMNttjNIjdASoIdrzD[seq(from = BylVf9ZwATNwlZKTFBny, to = BylVf9ZwATNwlZKTFBny + gCEORdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCEORdhbXDuADvpHQEgF)]
  }
  yhAsOHbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAsOHbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNiy <- matrix(NA, length(pJKoqYsmrWnGaouxiXfl)-NsnUWondQNfJtTsPDYXw+1-gCEORdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKLS7naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNiy)) {
    ExWMHMGVBDoquMmzzNiy[vgLdiovFqKLS7naKcrug,] <- pJKoqYsmrWnGaouxiXfl[seq(from = vgLdiovFqKLS7naKcrug, to = vgLdiovFqKLS7naKcrug + gCEORdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCEORdhbXDuADvpHQEgF)]
  }
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  ExWMHMGVBDoquMmzzNiy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNiy)))
  rDUOAeVAJmAdOZmDROBY <- matrix(NA, length(usSMNttjNIjdASoIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAdOZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdASoIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdASoIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  OMfAyvNomJPGWFkNPOyS = rDUOAeVAJmAdOZmDROBY
  qvTzKDylomztIVrjPiTW = ifelse(OMfAyvNomJPGWFkNPOyS[,2]>OMfAyvNomJPGWFkNPOyS[,1],3,ifelse(OMfAyvNomJPGWFkNPOyS[,2]<OMfAyvNomJPGWFkNPOyS[,1],1,2))
  XzDDQBYIdzInFewPvEoz <- matrix(NA, length(qvTzKDylomztIVrjPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiGCrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzInFewPvEoz[,ottwfdvTYtXqiGCrHRPL] <- qvTzKDylomztIVrjPiTW[(1+ottwfdvTYtXqiGCrHRPL-1):(length(qvTzKDylomztIVrjPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiGCrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzInFewPvEoz
  NdMDONYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdASoIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMDONYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMDONYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMDONYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMDONYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFI <- as.matrix(NdMDONYnSwnXqgqACWYn)
  StToiQjOWxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxiXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoQgj in 1:2) {
    StToiQjOWxhwYNNfkuSR[,LkisvFqdevaJYXTKoQgj] <- pJKoqYsmrWnGaouxiXfl[(1+LkisvFqdevaJYXTKoQgj-1):(length(pJKoqYsmrWnGaouxiXfl)-2+LkisvFqdevaJYXTKoQgj)]
    JUqQCBvVyDaXhVaVvvOu <- metric
  }
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  FLvyKjuktGFQMVcmvqVH = ifelse(StToiQjOWxhwYNNfkuSR[,2]>StToiQjOWxhwYNNfkuSR[,1],3,ifelse(StToiQjOWxhwYNNfkuSR[,2]<StToiQjOWxhwYNNfkuSR[,1],1,2))
  hclVQrmQLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFQMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFESmqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxkEvyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNiy), ncol = 1))
    hclVQrmQLGdqrRDBOORF[,QfHCVnZACUmjwVFESmqR] <- FLvyKjuktGFQMVcmvqVH[(1+QfHCVnZACUmjwVFESmqR-1):(length(FLvyKjuktGFQMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFESmqR)]
  }
  OSozUbQewSxUJkEvrpzW = hclVQrmQLGdqrRDBOORF
  HXnMePJMicyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxiXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMicyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMicyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMicyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAsOHbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMicyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMicyCtTGNoDuK)
  for (vghUNjcyidNBVbyISFVe in 1:nrow(yhAsOHbHiyZyIeejxKlc)) {
    yihwXLfhIkixEsKIFkla <- yhAsOHbHiyZyIeejxKlc[vghUNjcyidNBVbyISFVe,]
    IgLNhgCSdkaBerEqsGUE <- (yihwXLfhIkixEsKIFkla[-1]-yihwXLfhIkixEsKIFkla[-length(yihwXLfhIkixEsKIFkla)])
    IgLNhgCSdkaBerEqsGUE[is.nan(IgLNhgCSdkaBerEqsGUE)] <- 0
    IgLNhgCSdkaBerEqsGUE[is.infinite(IgLNhgCSdkaBerEqsGUE)] <- 0
    IgLNhgCSdkaBerEqsGUE[is.na(IgLNhgCSdkaBerEqsGUE)] <- 0
    ngHztivXaHUgAyOwSpol[,vghUNjcyidNBVbyISFVe] <- IgLNhgCSdkaBerEqsGUE
    zdjMaOZoftDoTNEaQPuW <- ExWMHMGVBDoquMmzzNiy[vghUNjcyidNBVbyISFVe,]
    kDrGpWDhvaxemdIrHoRq <- (zdjMaOZoftDoTNEaQPuW[-1]-zdjMaOZoftDoTNEaQPuW[-length(zdjMaOZoftDoTNEaQPuW)])
    kDrGpWDhvaxemdIrHoRq[is.nan(kDrGpWDhvaxemdIrHoRq)] <- 0
    kDrGpWDhvaxemdIrHoRq[is.infinite(kDrGpWDhvaxemdIrHoRq)] <- 0
    kDrGpWDhvaxemdIrHoRq[is.na(kDrGpWDhvaxemdIrHoRq)] <- 0
    XvINxkEvyNqYZCParXvC[,vghUNjcyidNBVbyISFVe] <- kDrGpWDhvaxemdIrHoRq
  }
  DNTYoyItXVqvBOeqgjMX <- as.matrix(dist(yhAsOHbHiyZyIeejxKlc, JUqQCBvVyDaXhVaVvvOu, upper=T))
  tOjcMmiYnSfibytlLxdV <- as.matrix(dist(ExWMHMGVBDoquMmzzNiy, JUqQCBvVyDaXhVaVvvOu, upper=T))
  PIWbQpdfoFDoOQZQWCWH = iRJdEuwumgRyLOihPcuQ
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    pJKoqYsmrWnGaouxiXfl <- Y
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    pJKoqYsmrWnGaouxiXfl <- Y
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  gCEORdhbXDuADvpHQEgF <- tau
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    usSMNttjNIjdASoIdrzD <- X 
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      gCEORdhbXDuADvpHQEgF <- tau
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  yhAsOHbHiyZyIeejxKlc <- matrix(NA, length(usSMNttjNIjdASoIdrzD)-NsnUWondQNfJtTsPDYXw+1-gCEORdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(BylVf9ZwATNwlZKTFBny in 1:nrow(yhAsOHbHiyZyIeejxKlc)) {
    yhAsOHbHiyZyIeejxKlc[BylVf9ZwATNwlZKTFBny,] <- usSMNttjNIjdASoIdrzD[seq(from = BylVf9ZwATNwlZKTFBny, to = BylVf9ZwATNwlZKTFBny + gCEORdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCEORdhbXDuADvpHQEgF)]
  }
  yhAsOHbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAsOHbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNiy <- matrix(NA, length(pJKoqYsmrWnGaouxiXfl)-NsnUWondQNfJtTsPDYXw+1-gCEORdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKLS7naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNiy)) {
    ExWMHMGVBDoquMmzzNiy[vgLdiovFqKLS7naKcrug,] <- pJKoqYsmrWnGaouxiXfl[seq(from = vgLdiovFqKLS7naKcrug, to = vgLdiovFqKLS7naKcrug + gCEORdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCEORdhbXDuADvpHQEgF)]
  }
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  ExWMHMGVBDoquMmzzNiy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNiy)))
  rDUOAeVAJmAdOZmDROBY <- matrix(NA, length(usSMNttjNIjdASoIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAdOZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdASoIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdASoIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  OMfAyvNomJPGWFkNPOyS = rDUOAeVAJmAdOZmDROBY
  qvTzKDylomztIVrjPiTW = ifelse(OMfAyvNomJPGWFkNPOyS[,2]>OMfAyvNomJPGWFkNPOyS[,1],3,ifelse(OMfAyvNomJPGWFkNPOyS[,2]<OMfAyvNomJPGWFkNPOyS[,1],1,2))
  XzDDQBYIdzInFewPvEoz <- matrix(NA, length(qvTzKDylomztIVrjPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiGCrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzInFewPvEoz[,ottwfdvTYtXqiGCrHRPL] <- qvTzKDylomztIVrjPiTW[(1+ottwfdvTYtXqiGCrHRPL-1):(length(qvTzKDylomztIVrjPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiGCrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzInFewPvEoz
  NdMDONYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdASoIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMDONYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMDONYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMDONYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMDONYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFI <- as.matrix(NdMDONYnSwnXqgqACWYn)
  StToiQjOWxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxiXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoQgj in 1:2) {
    StToiQjOWxhwYNNfkuSR[,LkisvFqdevaJYXTKoQgj] <- pJKoqYsmrWnGaouxiXfl[(1+LkisvFqdevaJYXTKoQgj-1):(length(pJKoqYsmrWnGaouxiXfl)-2+LkisvFqdevaJYXTKoQgj)]
    JUqQCBvVyDaXhVaVvvOu <- metric
  }
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  FLvyKjuktGFQMVcmvqVH = ifelse(StToiQjOWxhwYNNfkuSR[,2]>StToiQjOWxhwYNNfkuSR[,1],3,ifelse(StToiQjOWxhwYNNfkuSR[,2]<StToiQjOWxhwYNNfkuSR[,1],1,2))
  hclVQrmQLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFQMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFESmqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxkEvyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNiy), ncol = 1))
    hclVQrmQLGdqrRDBOORF[,QfHCVnZACUmjwVFESmqR] <- FLvyKjuktGFQMVcmvqVH[(1+QfHCVnZACUmjwVFESmqR-1):(length(FLvyKjuktGFQMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFESmqR)]
  }
  OSozUbQewSxUJkEvrpzW = hclVQrmQLGdqrRDBOORF
  HXnMePJMicyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxiXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMicyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMicyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMicyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAsOHbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMicyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMicyCtTGNoDuK)
  for (vghUNjcyidNBVbyISFVe in 1:nrow(yhAsOHbHiyZyIeejxKlc)) {
    yihwXLfhIkixEsKIFkla <- yhAsOHbHiyZyIeejxKlc[vghUNjcyidNBVbyISFVe,]
    IgLNhgCSdkaBerEqsGUE <- (yihwXLfhIkixEsKIFkla[-1]-yihwXLfhIkixEsKIFkla[-length(yihwXLfhIkixEsKIFkla)])
    IgLNhgCSdkaBerEqsGUE[is.nan(IgLNhgCSdkaBerEqsGUE)] <- 0
    IgLNhgCSdkaBerEqsGUE[is.infinite(IgLNhgCSdkaBerEqsGUE)] <- 0
    IgLNhgCSdkaBerEqsGUE[is.na(IgLNhgCSdkaBerEqsGUE)] <- 0
    ngHztivXaHUgAyOwSpol[,vghUNjcyidNBVbyISFVe] <- IgLNhgCSdkaBerEqsGUE
    zdjMaOZoftDoTNEaQPuW <- ExWMHMGVBDoquMmzzNiy[vghUNjcyidNBVbyISFVe,]
    kDrGpWDhvaxemdIrHoRq <- (zdjMaOZoftDoTNEaQPuW[-1]-zdjMaOZoftDoTNEaQPuW[-length(zdjMaOZoftDoTNEaQPuW)])
    kDrGpWDhvaxemdIrHoRq[is.nan(kDrGpWDhvaxemdIrHoRq)] <- 0
    kDrGpWDhvaxemdIrHoRq[is.infinite(kDrGpWDhvaxemdIrHoRq)] <- 0
    kDrGpWDhvaxemdIrHoRq[is.na(kDrGpWDhvaxemdIrHoRq)] <- 0
    XvINxkEvyNqYZCParXvC[,vghUNjcyidNBVbyISFVe] <- kDrGpWDhvaxemdIrHoRq
  }
  DNTYoyItXVqvBOeqgjMX <- as.matrix(dist(yhAsOHbHiyZyIeejxKlc, JUqQCBvVyDaXhVaVvvOu, upper=T))
  tOjcMmiYnSfibytlLxdV <- as.matrix(dist(ExWMHMGVBDoquMmzzNiy, JUqQCBvVyDaXhVaVvvOu, upper=T))
  PIWbQpdfoFDoOQZQWCWH = iRJdEuwumgRyLOihPcuQ
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    pJKoqYsmrWnGaouxiXfl <- Y
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    pJKoqYsmrWnGaouxiXfl <- Y
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  gCEORdhbXDuADvpHQEgF <- tau
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    usSMNttjNIjdASoIdrzD <- X 
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      gCEORdhbXDuADvpHQEgF <- tau
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  yhAsOHbHiyZyIeejxKlc <- matrix(NA, length(usSMNttjNIjdASoIdrzD)-NsnUWondQNfJtTsPDYXw+1-gCEORdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(BylVf9ZwATNwlZKTFBny in 1:nrow(yhAsOHbHiyZyIeejxKlc)) {
    yhAsOHbHiyZyIeejxKlc[BylVf9ZwATNwlZKTFBny,] <- usSMNttjNIjdASoIdrzD[seq(from = BylVf9ZwATNwlZKTFBny, to = BylVf9ZwATNwlZKTFBny + gCEORdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCEORdhbXDuADvpHQEgF)]
  }
  yhAsOHbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAsOHbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNiy <- matrix(NA, length(pJKoqYsmrWnGaouxiXfl)-NsnUWondQNfJtTsPDYXw+1-gCEORdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKLS7naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNiy)) {
    ExWMHMGVBDoquMmzzNiy[vgLdiovFqKLS7naKcrug,] <- pJKoqYsmrWnGaouxiXfl[seq(from = vgLdiovFqKLS7naKcrug, to = vgLdiovFqKLS7naKcrug + gCEORdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCEORdhbXDuADvpHQEgF)]
  }
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  ExWMHMGVBDoquMmzzNiy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNiy)))
  rDUOAeVAJmAdOZmDROBY <- matrix(NA, length(usSMNttjNIjdASoIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAdOZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdASoIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdASoIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  OMfAyvNomJPGWFkNPOyS = rDUOAeVAJmAdOZmDROBY
  qvTzKDylomztIVrjPiTW = ifelse(OMfAyvNomJPGWFkNPOyS[,2]>OMfAyvNomJPGWFkNPOyS[,1],3,ifelse(OMfAyvNomJPGWFkNPOyS[,2]<OMfAyvNomJPGWFkNPOyS[,1],1,2))
  XzDDQBYIdzInFewPvEoz <- matrix(NA, length(qvTzKDylomztIVrjPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiGCrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzInFewPvEoz[,ottwfdvTYtXqiGCrHRPL] <- qvTzKDylomztIVrjPiTW[(1+ottwfdvTYtXqiGCrHRPL-1):(length(qvTzKDylomztIVrjPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiGCrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzInFewPvEoz
  NdMDONYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdASoIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMDONYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMDONYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMDONYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMDONYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFI <- as.matrix(NdMDONYnSwnXqgqACWYn)
  StToiQjOWxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxiXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoQgj in 1:2) {
    StToiQjOWxhwYNNfkuSR[,LkisvFqdevaJYXTKoQgj] <- pJKoqYsmrWnGaouxiXfl[(1+LkisvFqdevaJYXTKoQgj-1):(length(pJKoqYsmrWnGaouxiXfl)-2+LkisvFqdevaJYXTKoQgj)]
    JUqQCBvVyDaXhVaVvvOu <- metric
  }
  
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  FLvyKjuktGFQMVcmvqVH = ifelse(StToiQjOWxhwYNNfkuSR[,2]>StToiQjOWxhwYNNfkuSR[,1],3,ifelse(StToiQjOWxhwYNNfkuSR[,2]<StToiQjOWxhwYNNfkuSR[,1],1,2))
  hclVQrmQLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFQMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFESmqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxkEvyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNiy), ncol = 1))
    hclVQrmQLGdqrRDBOORF[,QfHCVnZACUmjwVFESmqR] <- FLvyKjuktGFQMVcmvqVH[(1+QfHCVnZACUmjwVFESmqR-1):(length(FLvyKjuktGFQMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFESmqR)]
  }
  OSozUbQewSxUJkEvrpzW = hclVQrmQLGdqrRDBOORF
  HXnMePJMicyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxiXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMicyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMicyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMicyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAsOHbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMicyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMicyCtTGNoDuK)
  for (vghUNjcyidNBVbyISFVe in 1:nrow(yhAsOHbHiyZyIeejxKlc)) {
    yihwXLfhIkixEsKIFkla <- yhAsOHbHiyZyIeejxKlc[vghUNjcyidNBVbyISFVe,]
    IgLNhgCSdkaBerEqsGUE <- (yihwXLfhIkixEsKIFkla[-1]-yihwXLfhIkixEsKIFkla[-length(yihwXLfhIkixEsKIFkla)])
    IgLNhgCSdkaBerEqsGUE[is.nan(IgLNhgCSdkaBerEqsGUE)] <- 0
    IgLNhgCSdkaBerEqsGUE[is.infinite(IgLNhgCSdkaBerEqsGUE)] <- 0
    IgLNhgCSdkaBerEqsGUE[is.na(IgLNhgCSdkaBerEqsGUE)] <- 0
    ngHztivXaHUgAyOwSpol[,vghUNjcyidNBVbyISFVe] <- IgLNhgCSdkaBerEqsGUE
    zdjMaOZoftDoTNEaQPuW <- ExWMHMGVBDoquMmzzNiy[vghUNjcyidNBVbyISFVe,]
    kDrGpWDhvaxemdIrHoRq <- (zdjMaOZoftDoTNEaQPuW[-1]-zdjMaOZoftDoTNEaQPuW[-length(zdjMaOZoftDoTNEaQPuW)])
    kDrGpWDhvaxemdIrHoRq[is.nan(kDrGpWDhvaxemdIrHoRq)] <- 0
    kDrGpWDhvaxemdIrHoRq[is.infinite(kDrGpWDhvaxemdIrHoRq)] <- 0
    kDrGpWDhvaxemdIrHoRq[is.na(kDrGpWDhvaxemdIrHoRq)] <- 0
    XvINxkEvyNqYZCParXvC[,vghUNjcyidNBVbyISFVe] <- kDrGpWDhvaxemdIrHoRq
  }
  DNTYoyItXVqvBOeqgjMX <- as.matrix(dist(yhAsOHbHiyZyIeejxKlc, JUqQCBvVyDaXhVaVvvOu, upper=T))
  tOjcMmiYnSfibytlLxdV <- as.matrix(dist(ExWMHMGVBDoquMmzzNiy, JUqQCBvVyDaXhVaVvvOu, upper=T))
  PIWbQpdfoFDoOQZQWCWH = iRJdEuwumgRyLOihPcuQ
  NsnUWondQNfJtTsPDYXw <- E
  if (NsnUWondQNfJtTsPDYXw==2) {
    ZyBoRefCszHaUHJatHza <- c(6,12,18)
  } else if (NsnUWondQNfJtTsPDYXw==3) {
    ZyBoRefCszHaUHJatHza <- c(30,36,42,54,60,66,78,84,90)
  }
  usSMNttjNIjdA5oIdrzD <- X 
  pJKoqYsmrWnGaouxlXfl <- Y
  gCE0RdhbXDuADvpHQEgF <- tau
  iRJdEuwumgRyL0ihPcuQ <- h
  yhAs0HbHiyZyIeejxKlc <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  iRJdEuwumgRyLOihPcuQ <- h
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    pJKoqYsmrWnGaouxiXfl <- Y
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    pJKoqYsmrWnGaouxiXfl <- Y
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  gCEORdhbXDuADvpHQEgF <- tau
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    usSMNttjNIjdASoIdrzD <- X 
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      gCEORdhbXDuADvpHQEgF <- tau
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMjcyCtTGNoDuK)
  yhAsOHbHiyZyIeejxKlc <- matrix(NA, length(usSMNttjNIjdASoIdrzD)-NsnUWondQNfJtTsPDYXw+1-gCEORdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(BylVf9ZwATNwlZKTFBny in 1:nrow(yhAsOHbHiyZyIeejxKlc)) {
    yhAsOHbHiyZyIeejxKlc[BylVf9ZwATNwlZKTFBny,] <- usSMNttjNIjdASoIdrzD[seq(from = BylVf9ZwATNwlZKTFBny, to = BylVf9ZwATNwlZKTFBny + gCEORdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCEORdhbXDuADvpHQEgF)]
  }
  yhAsOHbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAsOHbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNiy <- matrix(NA, length(pJKoqYsmrWnGaouxiXfl)-NsnUWondQNfJtTsPDYXw+1-gCEORdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKLS7naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNiy)) {
    ExWMHMGVBDoquMmzzNiy[vgLdiovFqKLS7naKcrug,] <- pJKoqYsmrWnGaouxiXfl[seq(from = vgLdiovFqKLS7naKcrug, to = vgLdiovFqKLS7naKcrug + gCEORdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCEORdhbXDuADvpHQEgF)]
  }
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  ExWMHMGVBDoquMmzzNiy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNiy)))
  rDUOAeVAJmAdOZmDROBY <- matrix(NA, length(usSMNttjNIjdASoIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAdOZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdASoIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdASoIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  OMfAyvNomJPGWFkNPOyS = rDUOAeVAJmAdOZmDROBY
  qvTzKDylomztIVrjPiTW = ifelse(OMfAyvNomJPGWFkNPOyS[,2]>OMfAyvNomJPGWFkNPOyS[,1],3,ifelse(OMfAyvNomJPGWFkNPOyS[,2]<OMfAyvNomJPGWFkNPOyS[,1],1,2))
  XzDDQBYIdzInFewPvEoz <- matrix(NA, length(qvTzKDylomztIVrjPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiGCrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzInFewPvEoz[,ottwfdvTYtXqiGCrHRPL] <- qvTzKDylomztIVrjPiTW[(1+ottwfdvTYtXqiGCrHRPL-1):(length(qvTzKDylomztIVrjPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiGCrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzInFewPvEoz
  NdMDONYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdASoIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMDONYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMDONYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMDONYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMDONYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFI <- as.matrix(NdMDONYnSwnXqgqACWYn)
  StToiQjOWxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxiXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoQgj in 1:2) {
    StToiQjOWxhwYNNfkuSR[,LkisvFqdevaJYXTKoQgj] <- pJKoqYsmrWnGaouxiXfl[(1+LkisvFqdevaJYXTKoQgj-1):(length(pJKoqYsmrWnGaouxiXfl)-2+LkisvFqdevaJYXTKoQgj)]
    JUqQCBvVyDaXhVaVvvOu <- metric
  }
  for(BylVf97wATNwlZKTFBny in 1:nrow(yhAs0HbHiyZyIeejxKlc)) {
    yhAs0HbHiyZyIeejxKlc[BylVf97wATNwlZKTFBny,] <- usSMNttjNIjdA5oIdrzD[seq(from = BylVf97wATNwlZKTFBny, to = BylVf97wATNwlZKTFBny + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  yhAs0HbHiyZyIeejxKlc <- as.matrix(na.omit(data.frame(yhAs0HbHiyZyIeejxKlc)))
  ExWMHMGVBDoquMmzzNjy <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1-gCE0RdhbXDuADvpHQEgF+1, NsnUWondQNfJtTsPDYXw)
  for(vgLdiovFqKL57naKcrug in 1:nrow(ExWMHMGVBDoquMmzzNjy)) {
    ExWMHMGVBDoquMmzzNjy[vgLdiovFqKL57naKcrug,] <- pJKoqYsmrWnGaouxlXfl[seq(from = vgLdiovFqKL57naKcrug, to = vgLdiovFqKL57naKcrug + gCE0RdhbXDuADvpHQEgF * (NsnUWondQNfJtTsPDYXw-1), by = gCE0RdhbXDuADvpHQEgF)]
  }
  ExWMHMGVBDoquMmzzNjy <- as.matrix(na.omit(data.frame(ExWMHMGVBDoquMmzzNjy)))
  rDUOAeVAJmAd0ZmDROBY <- matrix(NA, length(usSMNttjNIjdA5oIdrzD)-2+1, 2)
  for(gWHdtREXfqCqmUvhqNYz in 1:2) {
    rDUOAeVAJmAd0ZmDROBY[,gWHdtREXfqCqmUvhqNYz] <- usSMNttjNIjdA5oIdrzD[(1+gWHdtREXfqCqmUvhqNYz-1):(length(usSMNttjNIjdA5oIdrzD)-2+gWHdtREXfqCqmUvhqNYz)]
  }
  OMfAyvNomJPGWFkNP0yS = rDUOAeVAJmAd0ZmDROBY
  qvTzKDylomztIVriPiTW = ifelse(OMfAyvNomJPGWFkNP0yS[,2]>OMfAyvNomJPGWFkNP0yS[,1],3,ifelse(OMfAyvNomJPGWFkNP0yS[,2]<OMfAyvNomJPGWFkNP0yS[,1],1,2))
  XzDDQBYIdzlnFewPvEoz <- matrix(NA, length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(ottwfdvTYtXqiCGrHRPL in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XzDDQBYIdzlnFewPvEoz[,ottwfdvTYtXqiCGrHRPL] <- qvTzKDylomztIVriPiTW[(1+ottwfdvTYtXqiCGrHRPL-1):(length(qvTzKDylomztIVriPiTW)-(NsnUWondQNfJtTsPDYXw-1)+ottwfdvTYtXqiCGrHRPL)]
  }
  vrppxFVFGqyMuFoWtEiP = XzDDQBYIdzlnFewPvEoz
  NdMD0NYnSwnXqgqACWYn = numeric(length(usSMNttjNIjdA5oIdrzD)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (CFLdmxvioWjqtAqqxVXh in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[CFLdmxvioWjqtAqqxVXh]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[CFLdmxvioWjqtAqqxVXh] = veiSmUtAbBbwPBHWtfzE
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (dHYDKLbxHojvCijqCQlB in 1:length(NdMD0NYnSwnXqgqACWYn)) {
      veiSmUtAbBbwPBHWtfzE = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==1) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 30
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 36
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 42
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==2) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 54
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 60
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 66
          }
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,2]==3) {
          if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==1) {
            veiSmUtAbBbwPBHWtfzE = 78
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==2) {
            veiSmUtAbBbwPBHWtfzE = 84
          } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB,1]==3) {
            veiSmUtAbBbwPBHWtfzE = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==1) {
          veiSmUtAbBbwPBHWtfzE = 6
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==2) {
          veiSmUtAbBbwPBHWtfzE = 12
        } else if (vrppxFVFGqyMuFoWtEiP[dHYDKLbxHojvCijqCQlB]==3) {
          veiSmUtAbBbwPBHWtfzE = 18
        }
      }
      NdMD0NYnSwnXqgqACWYn[dHYDKLbxHojvCijqCQlB] = veiSmUtAbBbwPBHWtfzE
    }
  }
  EiicsXdVMaQLWbhrJeFl <- as.matrix(NdMD0NYnSwnXqgqACWYn)
  StToiQj0WxhwYNNfkuSR <- matrix(NA, length(pJKoqYsmrWnGaouxlXfl)-2+1, 2)
  for(LkisvFqdevaJYXTKoOgj in 1:2) {
    StToiQj0WxhwYNNfkuSR[,LkisvFqdevaJYXTKoOgj] <- pJKoqYsmrWnGaouxlXfl[(1+LkisvFqdevaJYXTKoOgj-1):(length(pJKoqYsmrWnGaouxlXfl)-2+LkisvFqdevaJYXTKoOgj)]
    JUqQCBvVyDaXhVaVvv0u <- metric
  }
  FLvyKjuktGFOMVcmvqVH = ifelse(StToiQj0WxhwYNNfkuSR[,2]>StToiQj0WxhwYNNfkuSR[,1],3,ifelse(StToiQj0WxhwYNNfkuSR[,2]<StToiQj0WxhwYNNfkuSR[,1],1,2))
  hclVQrmOLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFE5mqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxk3vyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNjy), ncol = 1))
    hclVQrmOLGdqrRDBOORF[,QfHCVnZACUmjwVFE5mqR] <- FLvyKjuktGFOMVcmvqVH[(1+QfHCVnZACUmjwVFE5mqR-1):(length(FLvyKjuktGFOMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFE5mqR)]
  }
  OSozUbQew5xUJkEvrpzW = hclVQrmOLGdqrRDBOORF
  HXnMePJMjcyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxlXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMjcyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAs0HbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQew5xUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMjcyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  FLvyKjuktGFQMVcmvqVH = ifelse(StToiQjOWxhwYNNfkuSR[,2]>StToiQjOWxhwYNNfkuSR[,1],3,ifelse(StToiQjOWxhwYNNfkuSR[,2]<StToiQjOWxhwYNNfkuSR[,1],1,2))
  hclVQrmQLGdqrRDBOORF <- matrix(NA, length(FLvyKjuktGFQMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+1, NsnUWondQNfJtTsPDYXw-1)
  for(QfHCVnZACUmjwVFESmqR in 1:(NsnUWondQNfJtTsPDYXw-1)) {
    XvINxkEvyNqYZCParXvC <- t(matrix(data = NA, nrow = nrow(ExWMHMGVBDoquMmzzNiy), ncol = 1))
    hclVQrmQLGdqrRDBOORF[,QfHCVnZACUmjwVFESmqR] <- FLvyKjuktGFQMVcmvqVH[(1+QfHCVnZACUmjwVFESmqR-1):(length(FLvyKjuktGFQMVcmvqVH)-(NsnUWondQNfJtTsPDYXw-1)+QfHCVnZACUmjwVFESmqR)]
  }
  OSozUbQewSxUJkEvrpzW = hclVQrmQLGdqrRDBOORF
  HXnMePJMicyCtTGNoDuK = numeric(length(pJKoqYsmrWnGaouxiXfl)-NsnUWondQNfJtTsPDYXw+1)
  if (NsnUWondQNfJtTsPDYXw==3) {
    for (cDXBQyuIwToaEBmYKUgi in 1:length(HXnMePJMicyCtTGNoDuK)) {
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==1) {
          if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==2) {
          if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,2]==3) {
          if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQewSxUJkEvrpzW[cDXBQyuIwToaEBmYKUgi]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMicyCtTGNoDuK[cDXBQyuIwToaEBmYKUgi] = NKnjdeSVjEIHmPRFuAoQ
    }
  } else if (NsnUWondQNfJtTsPDYXw==2) {
    for (scuZiMnYXIxPZgQztYbz in 1:length(HXnMePJMicyCtTGNoDuK)) {
      ngHztivXaHUgAyOwSpol <- t(matrix(data = NA, nrow = nrow(yhAsOHbHiyZyIeejxKlc), ncol = 1))
      NKnjdeSVjEIHmPRFuAoQ = 0
      if (NsnUWondQNfJtTsPDYXw==3) {
        if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==1) {
          if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 30
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 36
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 42
          }
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==2) {
          if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 54
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 60
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 66
          }
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,2]==3) {
          if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==1) {
            NKnjdeSVjEIHmPRFuAoQ = 78
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==2) {
            NKnjdeSVjEIHmPRFuAoQ = 84
          } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz,1]==3) {
            NKnjdeSVjEIHmPRFuAoQ = 90
          }
        }
      } else if (NsnUWondQNfJtTsPDYXw==2) {
        if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==1) {
          NKnjdeSVjEIHmPRFuAoQ = 6
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==2) {
          NKnjdeSVjEIHmPRFuAoQ = 12
        } else if (OSozUbQewSxUJkEvrpzW[scuZiMnYXIxPZgQztYbz]==3) {
          NKnjdeSVjEIHmPRFuAoQ = 18
        }
      }
      HXnMePJMicyCtTGNoDuK[scuZiMnYXIxPZgQztYbz] = NKnjdeSVjEIHmPRFuAoQ
    }
  }
  tJDHGyPKWzRbCrbsFxrW <- as.matrix(HXnMePJMicyCtTGNoDuK)
  for (vghUNjcyidNBVbyISFVe in 1:nrow(yhAsOHbHiyZyIeejxKlc)) {
    yihwXLfhIkixEsKIFkla <- yhAsOHbHiyZyIeejxKlc[vghUNjcyidNBVbyISFVe,]
    IgLNhgCSdkaBerEqsGUE <- (yihwXLfhIkixEsKIFkla[-1]-yihwXLfhIkixEsKIFkla[-length(yihwXLfhIkixEsKIFkla)])
    IgLNhgCSdkaBerEqsGUE[is.nan(IgLNhgCSdkaBerEqsGUE)] <- 0
    IgLNhgCSdkaBerEqsGUE[is.infinite(IgLNhgCSdkaBerEqsGUE)] <- 0
    IgLNhgCSdkaBerEqsGUE[is.na(IgLNhgCSdkaBerEqsGUE)] <- 0
    ngHztivXaHUgAyOwSpol[,vghUNjcyidNBVbyISFVe] <- IgLNhgCSdkaBerEqsGUE
    zdjMaOZoftDoTNEaQPuW <- ExWMHMGVBDoquMmzzNiy[vghUNjcyidNBVbyISFVe,]
    kDrGpWDhvaxemdIrHoRq <- (zdjMaOZoftDoTNEaQPuW[-1]-zdjMaOZoftDoTNEaQPuW[-length(zdjMaOZoftDoTNEaQPuW)])
    kDrGpWDhvaxemdIrHoRq[is.nan(kDrGpWDhvaxemdIrHoRq)] <- 0
    kDrGpWDhvaxemdIrHoRq[is.infinite(kDrGpWDhvaxemdIrHoRq)] <- 0
    kDrGpWDhvaxemdIrHoRq[is.na(kDrGpWDhvaxemdIrHoRq)] <- 0
    XvINxkEvyNqYZCParXvC[,vghUNjcyidNBVbyISFVe] <- kDrGpWDhvaxemdIrHoRq
  }
  DNTYoyItXVqvBOeqgjMX <- as.matrix(dist(yhAsOHbHiyZyIeejxKlc, JUqQCBvVyDaXhVaVvvOu, upper=T))
  tOjcMmiYnSfibytlLxdV <- as.matrix(dist(ExWMHMGVBDoquMmzzNiy, JUqQCBvVyDaXhVaVvvOu, upper=T))
  PIWbQpdfoFDoOQZQWCWH = iRJdEuwumgRyLOihPcuQ
  AGELaUXqzRbncOnmTaFz <- (1 + NsnUWondQNfJtTsPDYXw + 1 + (NsnUWondQNfJtTsPDYXw-1)*gCEORdhbXDuADvpHQEgF + PIWbQpdfoFDoOQZQWCWH):(length(usSMNttjNIjdASoIdrzD)-(NsnUWondQNfJtTsPDYXw-1)*gCEORdhbXDuADvpHQEgF-iRJdEuwumgRyLOihPcuQ)
  qHkyybdqFHvyWKXPJNuW <- array(NA,dim = c(3^(NsnUWondQNfJtTsPDYXw-1),3^(NsnUWondQNfJtTsPDYXw-1),length(AGELaUXqzRbncOnmTaFz)))
  ocNvOzOOTCSABeXvWkIc <- 1
  for(pmnHpXIuwfpRyZJaMTyR in (1 + NsnUWondQNfJtTsPDYXw + 1 + (NsnUWondQNfJtTsPDYXw-1)*gCEORdhbXDuADvpHQEgF + PIWbQpdfoFDoOQZQWCWH):(nrow(yhAsOHbHiyZyIeejxKlc)-iRJdEuwumgRyLOihPcuQ)) {
    jHXhwNRdsbcNJMRPSHne <- DNTYoyItXVqvBOeqgjMX[pmnHpXIuwfpRyZJaMTyR,1:(pmnHpXIuwfpRyZJaMTyR-(NsnUWondQNfJtTsPDYXw-1)*gCEORdhbXDuADvpHQEgF-iRJdEuwumgRyLOihPcuQ)]
    eeEvwJiOvaSVEZJAiMgZ <- as.numeric(names(jHXhwNRdsbcNJMRPSHne[order(jHXhwNRdsbcNJMRPSHne)])[1:(NsnUWondQNfJtTsPDYXw+1)])
    mXnyzYVRYXJJmaTBDgER <- jHXhwNRdsbcNJMRPSHne[order(jHXhwNRdsbcNJMRPSHne)][1:(NsnUWondQNfJtTsPDYXw+1)]
    if (NsnUWondQNfJtTsPDYXw==3) {
      idoAyLZcmRmaWfISyNTA = ngHztivXaHUgAyOwSpol[eeEvwJiOvaSVEZJAiMgZ,]
    } else {
      idoAyLZcmRmaWfISyNTA = ngHztivXaHUgAyOwSpol[,eeEvwJiOvaSVEZJAiMgZ]
    }
    JXNsImDnbBwgNbneHVYq = mXnyzYVRYXJJmaTBDgER
    XTxzfQvokolxLbAPCMFs = sum(JXNsImDnbBwgNbneHVYq)
    if (XTxzfQvokolxLbAPCMFs==0) {
      XTxzfQvokolxLbAPCMFs <- 0.0001
    }
    ZGtNtPlUjLYdYiChWLsq = JXNsImDnbBwgNbneHVYq/XTxzfQvokolxLbAPCMFs
    qmrguAvTDoPvBTaETPnu <- exp(-ZGtNtPlUjLYdYiChWLsq)/sum(exp(-ZGtNtPlUjLYdYiChWLsq))
    praZYdSRdMAbotscstuC <- list("pmnHpXIuwfpRyZJaMTyR"=pmnHpXIuwfpRyZJaMTyR,
                                 "eeEvwJiOvaSVEZJAiMgZ"=eeEvwJiOvaSVEZJAiMgZ,
                                 "mXnyzYVRYXJJmaTBDgER"=mXnyzYVRYXJJmaTBDgER,
                                 "JXNsImDnbBwgNbneHVYq"=qmrguAvTDoPvBTaETPnu,
                                 "idoAyLZcmRmaWfISyNTA"=idoAyLZcmRmaWfISyNTA,
                                 "vXUmRYGaRGAORxFrqdCa"=EiicsXdVMaQLWbhrJeFI[eeEvwJiOvaSVEZJAiMgZ],
                                 "TEKKwtYaadwUgxxrwSwQ"=yhAsOHbHiyZyIeejxKlc[eeEvwJiOvaSVEZJAiMgZ,])
    htqPhQwWpGNfnVkqQoRi <- praZYdSRdMAbotscstuC
    JXNsImDnbBwgNbneHVYq = tOjcMmiYnSfibytlLxdV[pmnHpXIuwfpRyZJaMTyR,htqPhQwWpGNfnVkqQoRi$eeEvwJiOvaSVEZJAiMgZ+iRJdEuwumgRyLOihPcuQ]
    XTxzfQvokolxLbAPCMFs = sum(JXNsImDnbBwgNbneHVYq)
    if (XTxzfQvokolxLbAPCMFs==0) {
      XTxzfQvokolxLbAPCMFs <- 0.0001
    }
    ZGtNtPlUjLYdYiChWLsq = JXNsImDnbBwgNbneHVYq/XTxzfQvokolxLbAPCMFs
    qmrguAvTDoPvBTaETPnu <- exp(-ZGtNtPlUjLYdYiChWLsq)/sum(exp(-ZGtNtPlUjLYdYiChWLsq))
    if (NsnUWondQNfJtTsPDYXw==3) {
      idoAyLZcmRmaWfISyNTA = XvINxkEvyNqYZCParXvC[htqPhQwWpGNfnVkqQoRi$eeEvwJiOvaSVEZJAiMgZ+iRJdEuwumgRyLOihPcuQ,]
    } else {
      idoAyLZcmRmaWfISyNTA = XvINxkEvyNqYZCParXvC[,htqPhQwWpGNfnVkqQoRi$eeEvwJiOvaSVEZJAiMgZ+iRJdEuwumgRyLOihPcuQ]
    }
    WtwvCYsdTxyyLdlyCjJT <- list("pmnHpXIuwfpRyZJaMTyR+iRJdEuwumgRyLOihPcuQ"=pmnHpXIuwfpRyZJaMTyR+iRJdEuwumgRyLOihPcuQ,"eeEvwJiOvaSVEZJAiMgZ"=htqPhQwWpGNfnVkqQoRi$eeEvwJiOvaSVEZJAiMgZ+iRJdEuwumgRyLOihPcuQ,
                "mXnyzYVRYXJJmaTBDgER"=tOjcMmiYnSfibytlLxdV[pmnHpXIuwfpRyZJaMTyR,htqPhQwWpGNfnVkqQoRi$eeEvwJiOvaSVEZJAiMgZ+iRJdEuwumgRyLOihPcuQ],
                "JXNsImDnbBwgNbneHVYq"=as.vector(qmrguAvTDoPvBTaETPnu),
                "idoAyLZcmRmaWfISyNTA"=idoAyLZcmRmaWfISyNTA,
                "vXUmRYGaRGAORxFrqdCa"=tJDHGyPKWzRbCrbsFxrW[htqPhQwWpGNfnVkqQoRi$eeEvwJiOvaSVEZJAiMgZ+iRJdEuwumgRyLOihPcuQ],
                "TEKKwtYaadwUgxxrwSwQ"=ExWMHMGVBDoquMmzzNiy[htqPhQwWpGNfnVkqQoRi$eeEvwJiOvaSVEZJAiMgZ+iRJdEuwumgRyLOihPcuQ,])
    IkeldjWZvbvPEbYmsTVS <- WtwvCYsdTxyyLdlyCjJT
    if (NsnUWondQNfJtTsPDYXw == 3) {
      OBnCHfJqtOVbAtQmxSXd <- rep(0,NsnUWondQNfJtTsPDYXw-1)
      for (LkcEbETsuPmLoQSoiqbm in 1:length(OBnCHfJqtOVbAtQmxSXd)) {
        OBnCHfJqtOVbAtQmxSXd[LkcEbETsuPmLoQSoiqbm] <- ifelse(length(which(IkeldjWZvbvPEbYmsTVS$idoAyLZcmRmaWfISyNTA[,LkcEbETsuPmLoQSoiqbm]==0))>=(NsnUWondQNfJtTsPDYXw - 1),
                                            0,
                                            sum(IkeldjWZvbvPEbYmsTVS$idoAyLZcmRmaWfISyNTA[,LkcEbETsuPmLoQSoiqbm]*IkeldjWZvbvPEbYmsTVS$JXNsImDnbBwgNbneHVYq))
      }
    } else {
      OBnCHfJqtOVbAtQmxSXd <- ifelse(length(which(IkeldjWZvbvPEbYmsTVS$idoAyLZcmRmaWfISyNTA==0))>=(NsnUWondQNfJtTsPDYXw - 1),
                                    0,
                                    sum(IkeldjWZvbvPEbYmsTVS$idoAyLZcmRmaWfISyNTA*IkeldjWZvbvPEbYmsTVS$JXNsImDnbBwgNbneHVYq))
    }
    if (OBnCHfJqtOVbAtQmxSXd>0) {
      iBPSqkrvCuNlMXvwHttk <- 3
    } else if (OBnCHfJqtOVbAtQmxSXd<0) {
      iBPSqkrvCuNlMXvwHttk <- 1
    } else {
      iBPSqkrvCuNlMXvwHttk <- 2
    }
    QyqGDndTGJcAJtdosLOh = 0
    if (NsnUWondQNfJtTsPDYXw==3) {
      if (iBPSqkrvCuNlMXvwHttk[2]==1) {
        if (iBPSqkrvCuNlMXvwHttk[1]==1) {
          QyqGDndTGJcAJtdosLOh = 30
        } else if (iBPSqkrvCuNlMXvwHttk[1]==2) {
          QyqGDndTGJcAJtdosLOh = 36
        } else if (iBPSqkrvCuNlMXvwHttk[1]==3) {
          QyqGDndTGJcAJtdosLOh = 42
        }
      } else if (iBPSqkrvCuNlMXvwHttk[2]==2) {
        if (iBPSqkrvCuNlMXvwHttk[1]==1) {
          QyqGDndTGJcAJtdosLOh = 54
        } else if (iBPSqkrvCuNlMXvwHttk[1]==2) {
          QyqGDndTGJcAJtdosLOh = 60
        } else if (iBPSqkrvCuNlMXvwHttk[1]==3) {
          QyqGDndTGJcAJtdosLOh = 66
        }
      } else if (iBPSqkrvCuNlMXvwHttk[2]==3) {
        if (iBPSqkrvCuNlMXvwHttk[1]==1) {
          QyqGDndTGJcAJtdosLOh = 78
        } else if (iBPSqkrvCuNlMXvwHttk[1]==2) {
          QyqGDndTGJcAJtdosLOh = 84
        } else if (iBPSqkrvCuNlMXvwHttk[1]==3) {
          QyqGDndTGJcAJtdosLOh = 90
        }
      }
    } else if (NsnUWondQNfJtTsPDYXw==2) {
      if (iBPSqkrvCuNlMXvwHttk==1) {
        QyqGDndTGJcAJtdosLOh = 6
      } else if (iBPSqkrvCuNlMXvwHttk==2) {
        QyqGDndTGJcAJtdosLOh = 12
      } else if (iBPSqkrvCuNlMXvwHttk==3) {
        QyqGDndTGJcAJtdosLOh = 18
      }
    }
    aUmnKsSRuttrjCdaPeCO <- as.numeric(QyqGDndTGJcAJtdosLOh)
    if (NsnUWondQNfJtTsPDYXw == 3) {
      idoAyLZcmRmaWfISyNTAeX <- ngHztivXaHUgAyOwSpol[pmnHpXIuwfpRyZJaMTyR,] 
    } else {
      idoAyLZcmRmaWfISyNTAeX <- ngHztivXaHUgAyOwSpol[,pmnHpXIuwfpRyZJaMTyR]
    }
    HappwvtOMuEcFQvwZjKw <- EiicsXdVMaQLWbhrJeFI[pmnHpXIuwfpRyZJaMTyR,]
    if (NsnUWondQNfJtTsPDYXw == 3) {
      rhKPthBhTPxOtsRHEJto <- XvINxkEvyNqYZCParXvC[(pmnHpXIuwfpRyZJaMTyR+iRJdEuwumgRyLOihPcuQ),]
    } else {
      rhKPthBhTPxOtsRHEJto <- XvINxkEvyNqYZCParXvC[,(pmnHpXIuwfpRyZJaMTyR+iRJdEuwumgRyLOihPcuQ)]
    }
    QJRAqRuxozcdeXbtCpZf <- tJDHGyPKWzRbCrbsFxrW[pmnHpXIuwfpRyZJaMTyR,]
    if (length(aUmnKsSRuttrjCdaPeCO)>0) {
      if (length(HappwvtOMuEcFQvwZjKw)>0) {
        if (aUmnKsSRuttrjCdaPeCO==QJRAqRuxozcdeXbtCpZf) {
          QJRAqRuHbyjupTuLrsUeYpyEgBixozcdeXbtCpZf <- 2 * pnorm((sqrt(sum((OBnCHfJqtOVbAtQmxSXd)^2))/sqrt(sum((idoAyLZcmRmaWfISyNTAeX)^2)))* sqrt(2)) - 1
          ELSAKGjPkHliGHFuGHEd <- 2 * pnorm((sqrt(sum((rhKPthBhTPxOtsRHEJto)^2))/sqrt(sum((idoAyLZcmRmaWfISyNTAeX)^2)))* sqrt(2)) - 1
          QJRAqRuHbyjupTuLrsUeYpyEgBixozcdeXbtCpZf <- ifelse(QJRAqRuHbyjupTuLrsUeYpyEgBixozcdeXbtCpZf==0,1,QJRAqRuHbyjupTuLrsUeYpyEgBixozcdeXbtCpZf)
          QJRAqRuHbyjupTuLrsUeYpyEgBixozcdeXbtCpZf <- ifelse(is.nan(QJRAqRuHbyjupTuLrsUeYpyEgBixozcdeXbtCpZf),1,QJRAqRuHbyjupTuLrsUeYpyEgBixozcdeXbtCpZf)
          ELSAKGjPkHliGHFuGHEd <- ifelse(ELSAKGjPkHliGHFuGHEd==0,1,ELSAKGjPkHliGHFuGHEd)
          ELSAKGjPkHliGHFuGHEd <- ifelse(is.nan(ELSAKGjPkHliGHFuGHEd),1,ELSAKGjPkHliGHFuGHEd)
        } else {
          QJRAqRuHbyjupTuLrsUeYpyEgBixozcdeXbtCpZf <- NA
          ELSAKGjPkHliGHFuGHEd <- NA
        }
      } 
    } 
    MvJmVvHICJaBsTGOFDEH <- data.frame(sNwkCqGoDpLnKlchVopD=ELSAKGjPkHliGHFuGHEd,OHNMDbdXDfBYgZxdGmmW=QJRAqRuHbyjupTuLrsUeYpyEgBixozcdeXbtCpZf)
    qHkyybdqFHvyWKXPJNuW[which(ZyBoRefCszHaUHJatHza==HappwvtOMuEcFQvwZjKw),which(ZyBoRefCszHaUHJatHza==aUmnKsSRuttrjCdaPeCO),ocNvOzOOTCSABeXvWkIc] <- MvJmVvHICJaBsTGOFDEH$OHNMDbdXDfBYgZxdGmmW
    ocNvOzOOTCSABeXvWkIc <- ocNvOzOOTCSABeXvWkIc + 1
  }
  KPieGaFkjOqWdpOBgSSX <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  CRwyZIcXYIRRLJbvPFYJ <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  lkUyktDvyxUPbQtbjqOZ <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  jnhQdkVgiDMgggzkgtYF <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  for(XNdPCyXnyLlgLmScBcRz in 1:length(AGELaUXqzRbncOnmTaFz)) {
    KPKMWkPgGqEAMuvJZeDB <- which(qHkyybdqFHvyWKXPJNuW[,,XNdPCyXnyLlgLmScBcRz] != 0, arr.ind = TRUE)
    if (!is.na(qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz])) {
      if (!is.nan(qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz])) {
        if(!is.na(KPKMWkPgGqEAMuvJZeDB[1])) {
          if(!is.na(KPKMWkPgGqEAMuvJZeDB[2])) {
            if(KPKMWkPgGqEAMuvJZeDB[1]==KPKMWkPgGqEAMuvJZeDB[2]) {
              if (KPKMWkPgGqEAMuvJZeDB[1]!=mean(1:length(ZyBoRefCszHaUHJatHza))) {
                KPieGaFkjOqWdpOBgSSX[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              } else {
                lkUyktDvyxUPbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              }
            } else if ((KPKMWkPgGqEAMuvJZeDB[1]+KPKMWkPgGqEAMuvJZeDB[2])==(length(ZyBoRefCszHaUHJatHza)+1)) {
              if (KPKMWkPgGqEAMuvJZeDB[1]!=mean(1:length(ZyBoRefCszHaUHJatHza))) {
                CRwyZIcXYIRRLJbvPFYJ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              } else {
                lkUyktDvyxUPbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              }
            } else {
              lkUyktDvyxUPbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
            }
          }    else {
            jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
          }
        }    else {
          jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
        }
      }    else {
        jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
      }
    }    else {
      jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
    }
  }
  KPieGaFkjOqWdpOBgSSX <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  CRwyZIdXYIRRLJbvPFYJ <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  lkUyktDvyxUDbQtbjqOZ <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  jnhQdkVgiDMgggzkgtYF <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  for(XNdPCyXnyLlgLmScBcRz in 1:length(AGELaUXqzRbncOnmTaFz)) {
    KPKMWkPgGqEAMuvJZeDB <- which(qHkyybdqFHvyWKXPJNuW[,,XNdPCyXnyLlgLmScBcRz] != 0, arr.ind = TRUE)
    if (!is.na(qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz])) {
      if (!is.nan(qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz])) {
        if(!is.na(KPKMWkPgGqEAMuvJZeDB[1])) {
          if(!is.na(KPKMWkPgGqEAMuvJZeDB[2])) {
            if(KPKMWkPgGqEAMuvJZeDB[1]==KPKMWkPgGqEAMuvJZeDB[2]) {
              if (KPKMWkPgGqEAMuvJZeDB[1]!=mean(1:length(ZyBoRefCszHaUHJatHza))) {
                KPieGaFkjOqWdpOBgSSX[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              } else {
                lkUyktDvyxUDbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              }
            } else if ((KPKMWkPgGqEAMuvJZeDB[1]+KPKMWkPgGqEAMuvJZeDB[2])==(length(ZyBoRefCszHaUHJatHza)+1)) {
              if (KPKMWkPgGqEAMuvJZeDB[1]!=mean(1:length(ZyBoRefCszHaUHJatHza))) {
                CRwyZIdXYIRRLJbvPFYJ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              } else {
                lkUyktDvyxUDbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              }
            } else {
              lkUyktDvyxUDbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
            }
          }    else {
            jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
          }
        }    else {
          jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
        }
      }    else {
        jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
      }
    }    else {
      jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
    }
  }
  KPieGaFkjOqWdpOBgSSX <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  CRwyZIdXYIRRLJbvPFYJ <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  lkUyktDvyxUPbQtbjqOZ <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  jnhQdkVgiDMgggzkgtYF <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  for(XNdPCyXnyLlgLmScBcRz in 1:length(AGELaUXqzRbncOnmTaFz)) {
    KPKMWkPgGqEAMuvJZeDB <- which(qHkyybdqFHvyWKXPJNuW[,,XNdPCyXnyLlgLmScBcRz] != 0, arr.ind = TRUE)
    if (!is.na(qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz])) {
      if (!is.nan(qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz])) {
        if(!is.na(KPKMWkPgGqEAMuvJZeDB[1])) {
          if(!is.na(KPKMWkPgGqEAMuvJZeDB[2])) {
            if(KPKMWkPgGqEAMuvJZeDB[1]==KPKMWkPgGqEAMuvJZeDB[2]) {
              if (KPKMWkPgGqEAMuvJZeDB[1]!=mean(1:length(ZyBoRefCszHaUHJatHza))) {
                KPieGaFkjOqWdpOBgSSX[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              } else {
                lkUyktDvyxUPbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              }
            } else if ((KPKMWkPgGqEAMuvJZeDB[1]+KPKMWkPgGqEAMuvJZeDB[2])==(length(ZyBoRefCszHaUHJatHza)+1)) {
              if (KPKMWkPgGqEAMuvJZeDB[1]!=mean(1:length(ZyBoRefCszHaUHJatHza))) {
                CRwyZIdXYIRRLJbvPFYJ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              } else {
                lkUyktDvyxUPbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              }
            } else {
              lkUyktDvyxUPbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
            }
          }    else {
            jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
          }
        }    else {
          jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
        }
      }    else {
        jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
      }
    }    else {
      jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
    }
  }
  KPieGaFkiOqWdpOBgSSX <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  CRwyZIcXYIRRLJbvPFYJ <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  lkUyktDvyxUPbQtbjqOZ <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  jnhQdkVgiDMgggzkgtYF <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  for(XNdPCyXnyLlgLmScBcRz in 1:length(AGELaUXqzRbncOnmTaFz)) {
    KPKMWkPgGqEAMuvJZeDB <- which(qHkyybdqFHvyWKXPJNuW[,,XNdPCyXnyLlgLmScBcRz] != 0, arr.ind = TRUE)
    if (!is.na(qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz])) {
      if (!is.nan(qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz])) {
        if(!is.na(KPKMWkPgGqEAMuvJZeDB[1])) {
          if(!is.na(KPKMWkPgGqEAMuvJZeDB[2])) {
            if(KPKMWkPgGqEAMuvJZeDB[1]==KPKMWkPgGqEAMuvJZeDB[2]) {
              if (KPKMWkPgGqEAMuvJZeDB[1]!=mean(1:length(ZyBoRefCszHaUHJatHza))) {
                KPieGaFkiOqWdpOBgSSX[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              } else {
                lkUyktDvyxUPbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              }
            } else if ((KPKMWkPgGqEAMuvJZeDB[1]+KPKMWkPgGqEAMuvJZeDB[2])==(length(ZyBoRefCszHaUHJatHza)+1)) {
              if (KPKMWkPgGqEAMuvJZeDB[1]!=mean(1:length(ZyBoRefCszHaUHJatHza))) {
                CRwyZIcXYIRRLJbvPFYJ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              } else {
                lkUyktDvyxUPbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              }
            } else {
              lkUyktDvyxUPbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
            }
          }    else {
            jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
          }
        }    else {
          jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
        }
      }    else {
        jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
      }
    }    else {
      jnhQdkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
    }
  }
  KPieGaFkiOqWdpOBgSSX <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  CRwyZIcXYIRRLJbvPFYJ <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  lkUyktDvyxUPbQtbjqOZ <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  jnhQbkVgiDMgggzkgtYF <- vector(mode = "double", length = length(AGELaUXqzRbncOnmTaFz))
  for(XNdPCyXnyLlgLmScBcRz in 1:length(AGELaUXqzRbncOnmTaFz)) {
    KPKMWkPgGqEAMuvJZeDB <- which(qHkyybdqFHvyWKXPJNuW[,,XNdPCyXnyLlgLmScBcRz] != 0, arr.ind = TRUE)
    if (!is.na(qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz])) {
      if (!is.nan(qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz])) {
        if(!is.na(KPKMWkPgGqEAMuvJZeDB[1])) {
          if(!is.na(KPKMWkPgGqEAMuvJZeDB[2])) {
            if(KPKMWkPgGqEAMuvJZeDB[1]==KPKMWkPgGqEAMuvJZeDB[2]) {
              if (KPKMWkPgGqEAMuvJZeDB[1]!=mean(1:length(ZyBoRefCszHaUHJatHza))) {
                KPieGaFkiOqWdpOBgSSX[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              } else {
                lkUyktDvyxUPbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              }
            } else if ((KPKMWkPgGqEAMuvJZeDB[1]+KPKMWkPgGqEAMuvJZeDB[2])==(length(ZyBoRefCszHaUHJatHza)+1)) {
              if (KPKMWkPgGqEAMuvJZeDB[1]!=mean(1:length(ZyBoRefCszHaUHJatHza))) {
                CRwyZIcXYIRRLJbvPFYJ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              } else {
                lkUyktDvyxUPbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
              }
            } else {
              lkUyktDvyxUPbQtbjqOZ[XNdPCyXnyLlgLmScBcRz] <- qHkyybdqFHvyWKXPJNuW[KPKMWkPgGqEAMuvJZeDB[1],KPKMWkPgGqEAMuvJZeDB[2],XNdPCyXnyLlgLmScBcRz]
            }
          }    else {
            jnhQbkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
          }
        }    else {
          jnhQbkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
        }
      }    else {
        jnhQbkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
      }
    }    else {
      jnhQbkVgiDMgggzkgtYF[XNdPCyXnyLlgLmScBcRz] <- 1
    }
  }
  NOC <- data.frame(Positive=KPieGaFkiOqWdpOBgSSX,
                    Negative=CRwyZIcXYIRRLJbvPFYJ,
                    Dark=lkUyktDvyxUPbQtbjqOZ)
  return(NOC)
}


