#' Calculate Pattern Causality Effect Analysis
#' 
#' @title Pattern Causality Effect Analysis
#' @description Analyzes pattern causality matrices to compute and summarize the 
#' directional effects of different causality types (positive, negative, dark) 
#' between system components.
#'
#' @details
#' The function performs these key steps:
#' \itemize{
#'   \item Processes raw causality matrices
#'   \item Computes received and exerted influence for each component
#'   \item Calculates net causality effect (difference between received and exerted)
#'   \item Normalizes results to percentage scale
#' }
#'
#' @section Related Packages:
#' \itemize{
#'   \item \pkg{vars}: Vector autoregression for multivariate time series
#'   \item \pkg{lmtest}: Testing linear regression models
#'   \item \pkg{causality}: Causality testing and modeling
#' }
#'
#' @param pcmatrix An object of class "pc_matrix" containing causality matrices
#' @param verbose Logical; whether to display computation progress (default: FALSE)
#'
#' @return An object of class "pc_effect" containing:
#' \itemize{
#'   \item positive: Data frame of positive causality effects
#'   \item negative: Data frame of negative causality effects
#'   \item dark: Data frame of dark causality effects
#'   \item items: Vector of component names
#'   \item summary: Summary statistics for each causality type
#' }
#'
#' @examples
#' \donttest{
#' data(climate_indices)
#' dataset <- climate_indices[, -1]
#' pcmatrix <- pcMatrix(dataset, E = 3, tau = 1, 
#'                     metric = "euclidean", h = 1, 
#'                     weighted = TRUE)
#' effects <- pcEffect(pcmatrix)
#' print(effects)
#' plot(effects)
#' }
#'
#' @seealso 
#' \code{\link{pcMatrix}} for generating causality matrices
#' \code{\link{plot.pc_effect}} for visualizing causality effects
#'
#' @export
pcEffect <- function(pcmatrix, verbose = FALSE) {
  # Validate input
  if (!inherits(pcmatrix, "pc_matrix")) {
    stop("Input must be a pc_matrix object", call. = FALSE)
  }
  
  # Check if the matrix is square
  if (!isTRUE(pcmatrix$is_square)) {
    stop("Input pc_matrix object must have a square matrix. Use pcMatrix function instead of pcCrossMatrix.", call. = FALSE)
  }
  
  if (verbose) {
    cat("Processing causality matrices...\n")
  }
  
  # Initialize matrices with NA_real_
  matrices <- list(
    positive = replace(pcmatrix$positive, is.na(pcmatrix$positive), 0) * 100,
    negative = replace(pcmatrix$negative, is.na(pcmatrix$negative), 0) * 100,
    dark = replace(pcmatrix$dark, is.na(pcmatrix$dark), 0) * 100
  )
  
  # Compute effects
  effects <- lapply(matrices, function(m) {
    data.frame(
      received = rowSums(m),
      exerted = colSums(m),
      Diff = rowSums(m) - colSums(m),
      row.names = pcmatrix$items
    )
  })
  
  # Compute summary statistics
  summary_stats <- lapply(effects, function(df) {
    apply(df, 2, function(x) c(
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    ))
  })
  
  if (verbose) {
    cat("Computing summary statistics...\n")
  }
  
  # Create and return pc_effect object
  result <- structure(
    list(
      positive = effects$positive,
      negative = effects$negative,
      dark = effects$dark,
      items = pcmatrix$items,
      summary = summary_stats
    ),
    class = "pc_effect"
  )
  
  return(result)
}
