#' Validate inputs for pattern causality analysis
#' @keywords internal
#' @noRd
validate_inputs <- function(X, Y, E, tau, metric, h, weighted, distance_fn = NULL) {
  # Check vectors
  if(!is.numeric(X) || !is.numeric(Y)) {
    stop("X and Y must be numeric vectors", call. = FALSE)
  }
  if(length(X) != length(Y)) {
    stop("X and Y must have the same length", call. = FALSE)
  }
  if(any(is.infinite(X)) || any(is.infinite(Y))) {
    stop("X and Y cannot contain infinite values", call. = FALSE)
  }
  
  # Check parameters
  if(!is.numeric(E) || E <= 1 || E != round(E)) {
    stop("E must be a positive integer greater than 1", call. = FALSE)
  }
  if(!is.numeric(tau) || tau < 1 || tau != round(tau)) {
    stop("tau must be a positive integer", call. = FALSE)
  }
  
  # Only check metric if no custom distance function is provided
  if(is.null(distance_fn)) {
    if(!is.character(metric) || !metric %in% c("euclidean", "manhattan", "maximum")) {
      stop("metric must be one of: 'euclidean', 'manhattan', 'maximum'", call. = FALSE)
    }
  }
  
  if(!is.numeric(h) || h < 0 || h != round(h)) {
    stop("h must be a non-negative integer", call. = FALSE)
  }
  if(!is.logical(weighted)) {
    stop("weighted must be logical", call. = FALSE)
  }
  
  # Check data length requirements
  min_length <- (E - 1) * tau + h + 1
  if(length(X) < min_length) {
    stop(sprintf("Time series length must be at least %d for given E, tau and h", 
                min_length), call. = FALSE)
  }
  
  if(!is.null(distance_fn) && !is.function(distance_fn)) {
    stop("distance_fn must be a function", call. = FALSE)
  }
}

#' Compute state, signature and patterns
#' @keywords internal
#' @noRd  
compute_spaces <- function(X, Y, E, tau, metric,
                         distance_fn = NULL,
                         state_space_fn = NULL,
                         verbose = FALSE) {
  tryCatch({
    if(verbose) cat("Computing spaces...\n")
    
    # State Space
    if(verbose) cat("  - Computing state spaces... ")
    state_fn <- if(!is.null(state_space_fn)) state_space_fn else stateSpace
    Mx <- state_fn(X, E, tau)$matrix
    My <- state_fn(Y, E, tau)$matrix
    if(verbose) cat("Done\n")
    
    # Signature Space  
    if(verbose) cat("  - Computing signature spaces... ")
    SMx <- signatureSpace(Mx)
    SMy <- signatureSpace(My)
    if(verbose) cat("Done\n")
    
    # Pattern Space
    if(verbose) cat("  - Computing pattern spaces... ")
    PSMx <- patternSpace(SMx)
    PSMy <- patternSpace(SMy)
    if(verbose) cat("Done\n")
    
    # Distance Matrices
    if(verbose) cat("  - Computing distance matrices... ")
    dist_fn <- if(!is.null(distance_fn)) {
      function(x) if(!is.matrix(x)) as.matrix(distance_fn(x)) else distance_fn(x)
    } else {
      function(x) as.matrix(stats::dist(x, method = metric))
    }
    Dx <- dist_fn(Mx)
    Dy <- dist_fn(My)
    if(verbose) cat("Done\n")
    
    # Handle E=2 case
    if(E == 2) {
      SMx <- matrix(SMx, ncol = 1)
      SMy <- matrix(SMy, ncol = 1)
      PSMx <- matrix(PSMx, ncol = 1)
      PSMy <- matrix(PSMy, ncol = 1)
      if(!is.matrix(Mx)) Mx <- matrix(Mx, ncol = E)
      if(!is.matrix(My)) My <- matrix(My, ncol = E)
      if(!is.matrix(Dx)) Dx <- matrix(Dx, ncol = ncol(Mx))
      if(!is.matrix(Dy)) Dy <- matrix(Dy, ncol = ncol(My))
    }
    
    list(
      Mx = Mx, My = My,
      SMx = SMx, SMy = SMy,
      PSMx = PSMx, PSMy = PSMy,
      Dx = Dx, Dy = Dy
    )
  }, error = function(e) {
    stop("Error in space computation: ", e$message, call. = FALSE)
  })
}

#' Check causality points and determine analysis period
#' @keywords internal
#' @noRd
check_causality_points <- function(E, tau, h, X, verbose) {
  if(verbose) cat("Checking causality points...\n")
  
  # Check feasibility first
  check <- firstCausalityPointCHECK(E, tau, h, X)
  if(!check$feasible) {
    return(list(
      feasible = FALSE,
      FCP = NA_real_,
      al_loop_dur = NA_real_
    ))
  }
  
  # Calculate First Causality Point
  FCP <- firstCausalityPoint(E, tau, h, X)
  if(is.null(FCP) || is.na(FCP$point)) {
    return(list(
      feasible = FALSE,
      FCP = NA_real_,
      al_loop_dur = NA_real_
    ))
  }
  
  # Calculate loop duration
  loop_end <- length(X) - (E - 1) * tau - h
  if(FCP$point > loop_end) {
    return(list(
      feasible = FALSE,
      FCP = NA_real_,
      al_loop_dur = NA_real_
    ))
  }
  
  al_loop_dur <- FCP$point:loop_end
  
  list(
    feasible = TRUE,
    FCP = FCP$point,
    al_loop_dur = al_loop_dur
  )
}

#' Initialize matrices for pattern causality analysis
#' @keywords internal
#' @noRd
initialize_matrices <- function(X, Y, E, FCP, verbose) {
  if(verbose) cat("Initializing matrices...\n")
  
  n <- length(Y)
  pattern_dim <- 3^(E-1)
  
  # Initialize PC matrices
  predictedPCMatrix <- dataBank(type = "array", 
                              dimensions = c(pattern_dim, pattern_dim, n))
  realPCMatrix <- dataBank(type = "array", 
                          dimensions = c(pattern_dim, pattern_dim, n))
  
  # Initialize signature matrices
  predictedSignaturesY <- dataBank(type = "matrix", 
                                 dimensions = c(n, E-1))
  realSignaturesY <- dataBank(type = "matrix", 
                            dimensions = c(n, E-1))
  causalSignaturesX <- dataBank(type = "matrix", 
                              dimensions = c(n, E-1))
  
  # Initialize pattern vectors
  predictedPatternsY <- dataBank(type = "vector", dimensions = n)
  realPatternsY <- dataBank(type = "vector", dimensions = n)
  causalPatternsX <- dataBank(type = "vector", dimensions = n)
  
  # Initialize value matrices
  predictedValuesY <- structure(
    dataBank(type = "matrix", dimensions = c(n, E)),
    dimnames = list(NULL, c("currVal", rep("predVal", E-1)))
  )
  realValuesY <- structure(
    dataBank(type = "matrix", dimensions = c(n, E)),
    dimnames = list(NULL, c("currVal", rep("futuVal", E-1)))
  )
  
  list(
    pc_matrices = list(
      predicted = predictedPCMatrix,
      real = realPCMatrix
    ),
    signatures = list(
      predicted = predictedSignaturesY,
      real = realSignaturesY,
      causal = causalSignaturesX
    ),
    patterns = list(
      predicted = predictedPatternsY,
      real = realPatternsY,
      causal = causalPatternsX
    ),
    values = list(
      predicted = predictedValuesY,
      real = realValuesY
    )
  )
}

#' @export
print.pc_full_details <- function(x, ...) {
  cat("Pattern Causality Full Analysis\n")
  cat("------------------------------\n")
  
  # Analysis period
  cat("Analysis period:", range(x$backtest_time), "\n")
  cat("Number of valid observations:", length(x$valid_time), "\n")
  
  # Safely calculate causality range
  causality_values <- x$causality_real$total
  if (length(causality_values[!is.na(causality_values)]) > 0) {
    range_vals <- range(causality_values, na.rm = TRUE)
    cat("Causality spectrum range:", range_vals[1], range_vals[2], "\n")
  } else {
    cat("Causality spectrum range: No valid values\n")
  }
  
  # Space dimensions
  cat("\nState space dimensions:", 
      paste(dim(x$state_spaces$Mx), collapse = " x "), "\n")
  
  if (!is.null(x$matrices$predicted)) {
    cat("Pattern space dimensions:", 
        paste(dim(x$matrices$predicted), collapse = " x "), "\n")
  } else {
    cat("Pattern space dimensions: Not available\n")
  }
  
  cat("\n")
}

#' @export
summary.pc_full_details <- function(object, ...) {
  structure(
    list(
      analysis_period = range(object$backtest_time),
      valid_obs = length(object$valid_time),
      causality_stats = list(
        real = summary(object$causality_real$total),
        predicted = summary(object$causality_pred$total)
      ),
      prediction_accuracy = mean(
        object$causality_pred$total == object$causality_real$total, 
        na.rm = TRUE
      ),
      pattern_stats = list(
        unique_patterns = length(unique(object$patterns$real[!is.na(object$patterns$real)])),
        missing_patterns = sum(is.na(object$patterns$real))
      )
    ),
    class = "summary.pc_full_details"
  )
}

#' Plot Pattern Causality Time Series
#'
#' @description Visualizes the positive, negative and dark causality components over time
#' @param x A pc_full_details object
#' @param type The type of causality to plot ("total", "positive", "negative", or "dark")
#' @param ... Additional arguments passed to plotting functions
#' @return Invisibly returns the ggplot object
#' @export
plot_causality.pc_full_details <- function(x, type, ...) {
  samples <- series <- value <- value_interp <- NULL
  # First check weighted parameter and type
  if(!isTRUE(x$weighted)) {
    # Check type parameter is valid
    if(!type %in% c("total", "positive", "negative", "dark")) {
      stop("type must be one of: total, positive, negative, dark")
    }
    
    # Create sequence index
    idx <- seq_along(x$valid_time)
    
    # Create data frame for unweighted analysis
    data <- data.frame(
      samples = idx,
      nocausality = x$causality_real$no_causality[x$valid_time],
      positive = x$causality_real$positive[x$valid_time],
      negative = x$causality_real$negative[x$valid_time],
      dark = x$causality_real$dark[x$valid_time]
    )[, c("samples", "nocausality", "positive", "negative", "dark")]

    data_long <- tidyr::pivot_longer(data, 
                                   cols = c("nocausality", "positive", "negative", "dark"),
                                   names_to = "series", 
                                   values_to = "value")
    
    # Filter based on type
    if(type != "total") {
      data_long$value <- ifelse(data_long$series == type & data_long$value == 1, 1, NA)
    } else {
      data_long <- data_long[data_long$value == 1, ]
    }
    
    # Define colors for categories
    colors <- c(
      "nocausality" = "#DCDCDC",  # Light gray
      "positive" = "#5BA3CF",     # Blue
      "negative" = "#F6583E",     # Red
      "dark" = "#6A51A3"         # Purple
    )
    
    # Create plot
    p <- ggplot2::ggplot(data_long, ggplot2::aes(x = samples, fill = series)) +
      ggplot2::geom_col(ggplot2::aes(y = value), position = "stack", width = 1, na.rm = TRUE) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::labs(
        x = "Time", 
        y = "Causality Strength",
      ) +
      ggthemes::theme_few(base_size = 10) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.position = if(type == "total") "bottom" else "none",
        legend.title = ggplot2::element_blank(),
        legend.key.width = ggplot2::unit(1, "cm"),
        legend.background = ggplot2::element_rect(size = 0.2, 
                                                color = 'black', 
                                                linetype = 'solid')
      )
    
    print(p)
    invisible(p)
  } else {
    # Check type parameter is valid
    if(!type %in% c("total", "positive", "negative", "dark")) {
      stop("type must be one of: total, positive, negative, dark")
    }
    
    if(!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Package 'ggplot2' is required for plotting", call. = FALSE)
    }
    
    if(!requireNamespace("tidyr", quietly = TRUE)) {
      stop("Package 'tidyr' is required for plotting", call. = FALSE)
    }
    
    if(!requireNamespace("zoo", quietly = TRUE)) {
      stop("Package 'zoo' is required for plotting", call. = FALSE)
    }
    
    if(!requireNamespace("ggthemes", quietly = TRUE)) {
      stop("Package 'ggthemes' is required for plotting", call. = FALSE)
    }
    
    # Ensure data exists and is valid
    if(is.null(x$valid_time) || is.null(x$causality_real)) {
      stop("Invalid data structure: missing required components", call. = FALSE)
    }
    
    # Create sequence index
    idx <- seq_along(x$valid_time)
    
    # Check if E=2 and adjust data columns accordingly
    if(x$E == 2) {
      if(type == "dark") {
        stop("Dark causality plotting not available when E=2", call. = FALSE)
      }
      if(type == "total") {
        data_cols <- c("positive", "negative")  # Exclude dark for E=2
      } else {
        data_cols <- type
      }
    } else {
      if(type == "total") {
        data_cols <- c("positive", "negative", "dark")
      } else {
        data_cols <- type
      }
    }
    
    # Create initial data frame
    data <- data.frame(
      samples = idx,
      positive = x$causality_real$positive[x$valid_time],
      negative = x$causality_real$negative[x$valid_time],
      dark = x$causality_real$dark[x$valid_time]
    )[, c("samples", data_cols)]
    
    # Ensure no columns in the data frame are all NA
    if(any(sapply(data, function(x) all(is.na(x))))) {
      stop("Invalid data: contains columns with all NA values", call. = FALSE)
    }
    
    # Convert to long format
    data_long <- tidyr::pivot_longer(data, 
                                    cols = data_cols,
                                    names_to = "series", 
                                    values_to = "value")
    
    # Group-wise processing for each series
    series_list <- split(data_long, data_long$series)
    
    # Interpolate each series
    interpolated_series <- lapply(series_list, function(df) {
      non_zero_idx <- which(df$value > 0)
      if(length(non_zero_idx) > 1) {
        interp_fun <- stats::approx(
          x = df$samples[non_zero_idx],
          y = df$value[non_zero_idx],
          xout = df$samples,
          method = "linear",
          rule = 2
        )
        df$value_interp <- interp_fun$y
      } else {
        df$value_interp <- df$value
      }
      return(df)
    })
    
    # Combine interpolated data
    data_long <- do.call(rbind, interpolated_series)
    rownames(data_long) <- NULL
    
    # Define colors and shapes
    colors <- stats::setNames(
      c("#5BA3CF", "#F6583E", "#6A51A3"),
      c("positive", "negative", "dark")
    )
    shapes <- stats::setNames(
      c(16, 17, 15),  # circle, triangle, square
      c("positive", "negative", "dark")
    )
    
    # Create base plot
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(data = data_long, 
                        ggplot2::aes(x = samples, y = value_interp, 
                                    color = series, group = series), 
                        size = 1) +
      ggplot2::geom_point(data = subset(data_long, !is.na(value) & value > 0), 
                          ggplot2::aes(x = samples, y = value, 
                                      color = series, shape = series), 
                          size = 3) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_shape_manual(values = shapes) +
      ggplot2::labs(x = "Time", y = "Causality Strength") +
      ggthemes::theme_few(base_size = 10)
    
    # Add theme settings based on type
    if(type == "total") {
      p <- p + ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = ggplot2::element_blank(),
        legend.key.width = ggplot2::unit(1, "cm"),
        legend.background = ggplot2::element_rect(size = 0.2, 
                                                color = 'black', 
                                                linetype = 'solid')
      )
    } else {
      p <- p + ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.position = "none"
      )
    }
    
    print(p)
    invisible(p)
  }
}

#' Update matrices with new predictions and observations
#' @keywords internal
#' @noRd
update_matrices <- function(matrices, spaces, NNx, projNNy, i, h, weighted, verbose, hashedpatterns) {
  # Step 3: Predict pattern
  pred <- predictionY(projNNy)
  predictedSignatureY <- pred$signature
  predictedPatternY <- pred$pattern[1]
  
  # Step 4: Causal patterns
  signatureX <- spaces$SMx[i,]
  patternX <- spaces$PSMx[i]
  
  # Step 5: Real patterns
  realSignatureY <- spaces$SMy[i + h,]
  realPatternY <- spaces$PSMy[i + h]
  
  # Update matrices
  matrices$signatures$predicted[i,] <- predictedSignatureY
  matrices$signatures$real[i,] <- realSignatureY
  matrices$signatures$causal[i,] <- signatureX
  
  matrices$patterns$predicted[i] <- predictedPatternY
  matrices$patterns$real[i] <- realPatternY
  matrices$patterns$causal[i] <- patternX
  
  # Step 6: Update PC matrices
  pc <- fillPCMatrix(weighted, predictedPatternY, realPatternY, 
                    predictedSignatureY, realSignatureY, 
                    patternX, signatureX)
  
  if(!is.null(pc$predicted)) {
    matrices$pc_matrices$predicted[
      which(hashedpatterns == patternX),
      which(hashedpatterns == predictedPatternY),
      i
    ] <- pc$predicted
  }
  if(!is.null(pc$real)) {
    matrices$pc_matrices$real[
      which(hashedpatterns == patternX),
      which(hashedpatterns == predictedPatternY),
      i
    ] <- pc$real
  }
  
  matrices
}

#' Compute causality spectrums from PC matrices
#' @keywords internal
#' @noRd
compute_causality_spectrums <- function(pc_matrices, real_loop, hashedpatterns, X) {
  real <- natureOfCausality(pc_matrices$real, real_loop, hashedpatterns, X)
  predicted <- natureOfCausality(pc_matrices$predicted, real_loop, hashedpatterns, X)
  
  list(
    real = real,
    predicted = predicted
  )
}
#' Plot Pattern Causality Time Series
#'
#' @description Visualizes the positive, negative and dark causality components over time
#' @param x An object containing pattern causality results
#' @param type The type of causality to plot ("total", "positive", "negative", or "dark")
#' @param ... Additional arguments passed to plotting functions
#' @return Invisibly returns the ggplot object
#' @export
plot_causality <- function(x, type, ...) {
  if(missing(type)) {
    stop("'type' is a required parameter. Must be one of: total, positive, negative, dark")
  }
  UseMethod("plot_causality")
}

#' @export
plot_causality.default <- function(x, type, ...) {
  stop("plot_causality() is only implemented for pc_full_details objects")
}

#' Validate Custom Function Output
#' 
#' Validates the output format from custom distance and state space functions
#' 
#' @param output The output from a custom function to validate
#' @param fn_name The name of the function type being validated ("distance_fn" or "state_space_fn")
#' @return Nothing. Throws an error if validation fails.
#' @keywords internal
#' @export
validate_custom_fn_output <- function(output, fn_name) {
  if(fn_name == "distance_fn" && !is.matrix(output)) {
    stop("Custom distance function must return a matrix")
  }
  if(fn_name == "state_space_fn" && (!is.list(output) || !("matrix" %in% names(output)))) {
    stop("Custom state space function must return a list with 'matrix' element")
  }
}

#' Report Analysis Progress
#' @keywords internal
#' @noRd
report_progress <- function(current, total, prefix = "Progress", verbose = FALSE) {
  if (!verbose) return(invisible())
  msg <- sprintf("\r%s: %d/%d (%d%%)", 
                prefix, current, total, 
                round(100 * current/total))
  cat(msg)
  if (current == total) cat("\n")
}
