#' @title 3D State Space Plot for Phase Space Reconstruction
#' 
#' @description
#' The `plotState` function generates a 3D plot of the reconstructed state space for time series data with an embedding dimension of 3. It uses the `scatter3D_fancy` function to create a detailed 3D scatter plot, which helps visualize the dynamics of the system in three dimensions.
#' 
#' @importFrom plot3D trans3D scatter2D scatter3D
#' @param statedata A matrix or data frame containing the reconstructed state space data, where each column corresponds to a different dimension. The input must have exactly 3 columns, corresponding to an embedding dimension (`E`) of 3.
#' @param ... Some other settings for plot
#' @param style choose a style for the 3D plot
#' 
#' @return
#' A 3D scatter plot that visualizes the state space of the time series data. The plot provides insights into the underlying structure and behavior of the dynamical system being studied.
#' 
#' @export
#' @examples
#' \donttest{
#' data(climate_indices)
#' state_data <- stateSpace(climate_indices$AAO, E = 3, tau = 2)
#' plotState(state_data,phi=20, style=1)
#' }
plotState <- function(statedata, ..., style=2){
  if(dim(statedata)[2] != 3){
    stop("Your E is not equal to 3, please correct.")
  }
  if(style==2){
    scatter3D(statedata[,1], statedata[,2], statedata[,3], pch = 18, bty = "u", colkey = FALSE, 
              col.panel ="steelblue", expand =0.4,
              col.grid = "darkblue")
  }
  else if(style==1){
    scatter3D_fancy(statedata[,1], statedata[,2], statedata[,3], pch = 16,
                    xlab='',ylab='',zlab='',ticktype = "detailed", ...,
                    theta = 15, d = 2 )
  }
  else{
    stop("The style parameter should be 1 or 2.")
  }
}

## Help function
scatter3D_fancy <- function(x, y, z,..., colvar = z)
{
  panelfirst <- function(pmat) {
    XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
    
    XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
  }
  scatter3D(x, y, z, ..., colvar = colvar, panel.first=panelfirst,
            colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75)) 
}