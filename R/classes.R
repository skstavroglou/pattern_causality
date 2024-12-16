#' Plot Total Pattern Causality
#'
#' Visualizes the total pattern causality strength as a barplot.
#' This function takes a `pc_fit` object and generates a barplot showing the
#' overall causality strength.
#'
#' @param x An object containing pattern causality results, typically a `pc_fit` object.
#' @param ... Additional arguments passed to the underlying plotting functions.
#' @return NULL invisibly.
#' @seealso \code{\link{plot_components}} for visualizing individual causality components.
#' @references Stavroglou et al. (2020) \doi{10.1073/pnas.1918269117}
#' @examples
#' data(climate_indices)
#' X <- climate_indices$AO
#' Y <- climate_indices$AAO
#' pc_result <- pcLightweight(X, Y, E = 3, tau = 2, metric = "euclidean", h = 1, weighted = TRUE)
#' plot_total(pc_result)
#' @export
plot_total <- function(x, ...) {
  UseMethod("plot_total")
}

#' Plot Pattern Causality Components
#'
#' Visualizes the positive, negative, and dark causality components as a barplot.
#' This function takes a `pc_fit` object and generates a barplot showing the
#' strength of each causality component.
#'
#' @param x An object containing pattern causality results, typically a `pc_fit` object.
#' @param ... Additional arguments passed to the underlying plotting functions.
#' @return NULL invisibly.
#' @examples
#' data(climate_indices)
#' X <- climate_indices$AO
#' Y <- climate_indices$AAO
#' pc_result <- pcLightweight(X, Y, E = 3, tau = 2, metric = "euclidean", h = 1, weighted = TRUE)
#' plot_components(pc_result)
#' @export
plot_components <- function(x, ...) {
  UseMethod("plot_components")
}

#' Print Pattern Causality Results
#'
#' Prints the pattern causality analysis results from a `pc_fit` object.
#' This function displays the total, positive, negative, and dark causality strengths.
#'
#' @param x A `pc_fit` object.
#' @param ... Additional arguments passed to the `print` function.
#' @return Invisibly returns the input object.
#' @export
#' @method print pc_fit
print.pc_fit <- function(x, ...) {
  cat("Pattern Causality Analysis Results:\n")
  cat(sprintf("Total: %.4f\n", x$total))
  cat(sprintf("Positive: %.4f\n", x$positive))
  cat(sprintf("Negative: %.4f\n", x$negative))
  cat(sprintf("Dark: %.4f\n", x$dark))
  invisible(x)
}

#' Summarize Pattern Causality Results
#'
#' Provides a summary of the pattern causality analysis results from a `pc_fit` object.
#' This function displays a table of causality strengths for total, positive, negative, and dark components.
#'
#' @param object A `pc_fit` object.
#' @param ... Additional arguments passed to the `summary` function.
#' @return Invisibly returns the input object.
#' @export
#' @method summary pc_fit
summary.pc_fit <- function(object, ...) {
  cat("Pattern Causality Summary:\n\n")
  cat("Causality Strengths:\n")
  res <- data.frame(
    Type = c("Total", "Positive", "Negative", "Dark"),
    Value = c(object$total, object$positive, object$negative, object$dark)
  )
  print(res)
  invisible(object)
}

#' Plot Total Causality
#'
#' Visualizes the total causality strength as a barplot for a `pc_fit` object.
#' This function generates a barplot showing the total causality strength and its complement.
#'
#' @param x A `pc_fit` object.
#' @param ... Additional arguments passed to the underlying plotting functions.
#' @return NULL.
#' @export
#' @method plot_total pc_fit
plot_total.pc_fit <- function(x, ...) {
  data <- data.frame(
    name = c('Causality', 'Nocausality'),
    value = c(x$total, 1 - x$total)
  )
  coul <- c("#DCDCDC", "#696969")
  
  graphics::barplot(height = data$value,
          names.arg = data$name,
          col = coul,
          border = "white",
          ylim = c(0, 1),
          ylab = "Strength")
  
  graphics::text(x = graphics::barplot(height = data$value, plot = FALSE),
       y = data$value,
       label = paste0(round(data$value * 100, 1), "%"),
       pos = 3)
}

#' Plot Causality Components
#'
#' Visualizes the positive, negative, and dark causality components as a barplot for a `pc_fit` object.
#' This function generates a barplot showing the strength of each causality component.
#'
#' @param x A `pc_fit` object.
#' @param ... Additional arguments passed to the underlying plotting functions.
#' @return NULL.
#' @export
#' @method plot_components pc_fit
plot_components.pc_fit <- function(x, ...) {
  data <- data.frame(
    name = c('Positive', 'Negative', 'Dark'),
    value = c(x$positive, x$negative, x$dark)
  )
  coul <- c("#5BA3CF", "#F6583E", "#6A51A3")
  
  graphics::barplot(height = data$value,
          names.arg = data$name,
          col = coul,
          border = "white",
          ylim = c(0, max(data$value) * 1.2),
          ylab = "Strength")
  
  graphics::text(x = graphics::barplot(height = data$value, plot = FALSE),
       y = data$value,
       label = paste0(round(data$value * 100, 1), "%"),
       pos = 3)
}

#' Plot Pattern Causality Results
#'
#' Generates a combined plot of total causality and causality components for a `pc_fit` object.
#' This function combines the visualizations from `plot_total` and `plot_components` into a single plot.
#'
#' @param x A `pc_fit` object.
#' @param ... Additional arguments passed to the underlying plotting functions.
#' @return NULL invisibly.
#' @export
#' @method plot pc_fit
plot.pc_fit <- function(x, ...) {
  data1 <- data.frame(
    name = c('Positive', 'Negative', 'Dark'),
    value = c(x$positive, x$negative, x$dark)
  )
  coul1 <- c("#5BA3CF", "#F6583E", "#6A51A3")
  
  data2 <- data.frame(
    name = c('Causality', 'Nocausality'),
    value = c(x$total, 1 - x$total)
  )
  coul2 <- c("#DCDCDC", "#696969")
  
  graphics::par(mfrow = c(1, 2))
  
  graphics::barplot(height = data1$value,
          names.arg = data1$name,
          col = coul1,
          border = "white",
          ylim = c(0, max(data1$value) * 1.2),
          ylab = "Strength")
  
  graphics::text(x = graphics::barplot(height = data1$value, plot = FALSE),
       y = data1$value,
       label = paste0(round(data1$value * 100, 1), "%"),
       pos = 3)
  
  graphics::barplot(height = data2$value,
          names.arg = data2$name,
          col = coul2,
          border = "white",
          ylim = c(0, 1),
          ylab = "Strength")
  
  graphics::text(x = graphics::barplot(height = data2$value, plot = FALSE),
       y = data2$value,
       label = paste0(round(data2$value * 100, 1), "%"),
       pos = 3)
  
  invisible(NULL)
}

#' Pattern Causality Matrix Object
#'
#' Creates a pattern causality matrix object.
#' This function constructs an object of class `pc_matrix` containing the positive,
#' negative, and dark causality matrices, along with item names.
#'
#' @param positive Positive causality matrix.
#' @param negative Negative causality matrix.
#' @param dark Dark causality matrix.
#' @param items Names of items in the matrices.
#' @param verbose Logical, whether to print progress information.
#' @return An object of class "pc_matrix".
#' @examples
#' data(climate_indices)
#' dataset <- climate_indices[, -1]
#' pc_matrix_obj <- pcMatrix(dataset, E = 3, tau = 1, 
#'   metric = "euclidean", h = 1, weighted = TRUE, 
#'   verbose = FALSE)
#' print(pc_matrix_obj)
#' @export
pc_matrix <- function(positive = NULL, negative = NULL, dark = NULL, items = NULL, verbose = TRUE) {
  if(verbose) {
    cat("Creating Pattern Causality Matrix object...\n")
  }
  
  # Validate input matrices dimensions
  matrices <- list(positive = positive, negative = negative, dark = dark)
  dims <- lapply(matrices[!sapply(matrices, is.null)], dim)
  if(length(unique(dims)) > 1) {
    stop("All provided matrices must have the same dimensions")
  }
  
  # Get or validate items
  if(is.null(items)) {
    items <- colnames(positive)
    if(is.null(items)) {
      n <- nrow(positive)
      items <- paste0("V", seq_len(n))
    }
  } else if (is.list(items)) {
      # Handle list of items (from pcCrossMatrix)
      if (length(items) != 2) {
          stop("items list must have length 2 (X and Y names)")
      }
      items_X <- items[[1]]
      items_Y <- items[[2]]
      if (length(items_X) != nrow(positive) || length(items_Y) != ncol(positive)) {
          stop("Length of items in list must match matrix dimensions")
      }
      
  } else if(length(items) != nrow(positive)) {
    stop("Length of items must match matrix dimensions")
  }
  
  # Set row and column names for all matrices
  for(type in c("positive", "negative", "dark")) {
    if(!is.null(matrices[[type]])) {
        if (is.list(items)) {
            rownames(matrices[[type]]) <- items_X
            colnames(matrices[[type]]) <- items_Y
        } else {
            rownames(matrices[[type]]) <- items
            colnames(matrices[[type]]) <- items
        }
    }
  }
  
  # Create and return pc_matrix object
  structure(
    list(
      positive = matrices$positive,
      negative = matrices$negative,
      dark = matrices$dark,
      items = items
    ),
    class = "pc_matrix"
  )
}

#' Print Pattern Causality Matrix
#'
#' Prints the pattern causality matrix object.
#' This function displays the specified causality matrix (or all matrices)
#' with a preview of the first 5 rows and columns.
#'
#' @param x A `pc_matrix` object.
#' @param type The type of matrix to print ("all" or "positive", "negative", "dark").
#' @param ... Additional arguments passed to the `print` function.
#' @return Invisibly returns the input object.
#' @examples
#' data(climate_indices)
#' dataset <- climate_indices[, -1]
#' pc_matrix_obj <- pcMatrix(dataset, E = 3, tau = 1, 
#'   metric = "euclidean", h = 1, weighted = TRUE, 
#'   verbose = FALSE)
#' print(pc_matrix_obj, type = "positive")
#' @export
#' @method print pc_matrix
print.pc_matrix <- function(x, type = "all", ...) {
  # Validate type parameter
  valid_types <- c("all", "positive", "negative", "dark")
  if(!type %in% valid_types) {
    stop("type must be one of: all, positive, negative, dark")
  }
  
  cat("Pattern Causality Matrix Analysis:\n\n")
  cat("Number of items:", length(x$items), "\n")
  
  if(type == "all") {
    cat("\nPositive causality matrix:\n")
    print(x$positive[1:min(5, nrow(x$positive)), 1:min(5, ncol(x$positive))])
    if(nrow(x$positive) > 5) cat("...\n")
    
    cat("\nNegative causality matrix:\n")
    print(x$negative[1:min(5, nrow(x$negative)), 1:min(5, ncol(x$negative))])
    if(nrow(x$negative) > 5) cat("...\n")
    
    cat("\nDark causality matrix:\n")
    print(x$dark[1:min(5, nrow(x$dark)), 1:min(5, ncol(x$dark))])
    if(nrow(x$dark) > 5) cat("...\n")
  } else {
    cat(sprintf("\n%s causality matrix:\n", tools::toTitleCase(type)))
    matrix_to_print <- switch(type,
      positive = x$positive,
      negative = x$negative,
      dark = x$dark
    )
    print(matrix_to_print)
  }
  
  invisible(x)
}

#' Summarize Pattern Causality Matrix
#'
#' Provides a summary of the pattern causality matrix object.
#' This function calculates and displays descriptive statistics (mean, SD, min, max)
#' for each causality matrix (positive, negative, dark).
#'
#' @param object A `pc_matrix` object.
#' @param ... Additional arguments passed to the `summary` function.
#' @return Invisibly returns the input object.
#' @examples
#' data(climate_indices)
#' dataset <- climate_indices[, -1]
#' pc_matrix_obj <- pcMatrix(dataset, E = 3, tau = 1, 
#'   metric = "euclidean", h = 1, weighted = TRUE, 
#'   verbose = FALSE)
#' summary(pc_matrix_obj)
#' @export
#' @method summary pc_matrix
summary.pc_matrix <- function(object, ...) {
  cat("Pattern Causality Matrix Summary:\n\n")
  cat("Number of items:", length(object$items), "\n")
  cat("\nMatrix statistics:\n")
  
  stats_list <- list(
    Positive = compute_matrix_stats(object$positive),
    Negative = compute_matrix_stats(object$negative),
    Dark = compute_matrix_stats(object$dark)
  )
  
  stats_df <- data.frame(
    Type = names(stats_list),
    Mean = sapply(stats_list, function(x) round(x$mean, 4)),
    SD = sapply(stats_list, function(x) round(x$sd, 4)),
    Min = sapply(stats_list, function(x) round(x$min, 4)),
    Max = sapply(stats_list, function(x) round(x$max, 4))
  )
  
  print(stats_df)
  invisible(object)
}

#' Plot Pattern Causality Matrix
#'
#' Creates a heatmap visualization of the pattern causality matrix for positive,
#' negative, or dark causality relationships.
#' This function generates a heatmap using `ggplot2` to visualize the specified
#' causality matrix.
#'
#' @param x A `pc_matrix` object containing causality matrices.
#' @param status The type of causality to plot ("positive", "negative", or "dark").
#' @param width Numeric value specifying the width of the bars (default: 0.85).
#' @param height Numeric value specifying the height of the bars (default: 0.75).
#' @param radius Grid unit specifying the corner radius of the bars.
#' @param alpha Numeric value specifying the transparency (default: 0.53).
#' @param show_text Logical, whether to show numerical values on the plot.
#' @param show_legend_title Logical, whether to display the legend title.
#' @param ... Additional arguments passed to plotting functions.
#' @return A ggplot object invisibly.
#' @references Stavroglou et al. (2020) \doi{10.1073/pnas.1918119117}
#' @examples
#' data(climate_indices)
#' dataset <- climate_indices[, -1]
#' pc_matrix_obj <- pcMatrix(dataset, E = 3, tau = 1, 
#'   metric = "euclidean", h = 1, weighted = TRUE, 
#'   verbose = FALSE)
#' plot(pc_matrix_obj, status = "positive")
#' @export
#' @method plot pc_matrix
plot.pc_matrix <- function(x, status, 
                          width = 0.85,
                          height = 0.75,
                          radius = grid::unit(3, "pt"),
                          alpha = 0.53,
                          show_text = FALSE,
                          show_legend_title = FALSE,
                          ...) {
  # Check if status is missing
  X <- Y <- Value <- NULL
  if(missing(status)) {
    stop("'status' is a required parameter. Must be one of: positive, negative, dark")
  }
  
  if(!status %in% c("positive", "negative", "dark")) {
    stop("status must be one of: positive, negative, dark")
  }
  
  # Get the appropriate matrix
  matrix_to_plot <- switch(status,
    positive = x$positive,
    negative = x$negative,
    dark = x$dark
  )
  
  # Convert matrix to long format with proper handling of row/column names
  matrix_to_plot <- as.matrix(matrix_to_plot) # Ensure it's a matrix
  matrix_to_plot[is.na(matrix_to_plot)] <- 0  # Replace NA with 0
  
  # Reverse row order to flip matrix
  matrix_to_plot <- matrix_to_plot[nrow(matrix_to_plot):1, ]
  
  row_names <- rownames(matrix_to_plot)
  col_names <- colnames(matrix_to_plot)
  
  # Create data frame
  data_long <- reshape2::melt(matrix_to_plot)
  colnames(data_long) <- c("X", "Y", "Value")
  
  # Set color scheme based on status
  color_high <- switch(status,
    positive = RColorBrewer::brewer.pal(7, "Blues")[-1],
    negative = RColorBrewer::brewer.pal(7, "Reds")[-1],
    dark = RColorBrewer::brewer.pal(7, "Purples")[-1]
  )
  
  # Create plot
  p <- ggplot2::ggplot(data_long, ggplot2::aes(x = Y, y = X, fill = Value)) +
    geom_rtile(ggplot2::aes(width = width, height = height, fill = Value), 
               alpha = alpha, 
               radius = radius) +
    ggplot2::scale_fill_gradient(low = "#FFFFFF", 
                                high = color_high,
                                limits = c(0, max(matrix_to_plot)),
                                name = if(show_legend_title) status else "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(),
      legend.title = if(show_legend_title) 
                      ggplot2::element_text() 
                    else 
                      ggplot2::element_blank()
    ) +
    ggplot2::coord_fixed(ratio = 1.1)
  
  # Add text if requested  
  if(show_text) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = round(Value, 2)))
  }
  
  print(p)
  invisible(p)
}

#' Pattern Causality Cross-Validation Object
#'
#' Creates a pattern causality cross-validation object containing results
#' from repeated sampling analysis.
#' This function constructs an object of class `pc_cv` to store the results
#' of cross-validation analysis.
#'
#' @param samples Numeric vector of sample sizes used.
#' @param results Matrix containing causality results for each sample.
#' @param summary Matrix of summary statistics.
#' @param parameters List of analysis parameters.
#' @return An object of class "pc_cv".
#' @export
pc_cv <- function(samples = NULL, results = NULL, summary = NULL, parameters = NULL) {
  structure(
    list(
      samples = samples,
      results = results,
      summary = summary,
      parameters = parameters
    ),
    class = "pc_cv"
  )
}

#' Print Pattern Causality Cross Validation Results
#'
#' Prints the pattern causality cross-validation results.
#' This function displays the parameters used for cross-validation,
#' the sample sizes, and the summary statistics.
#'
#' @param x A `pc_cv` object.
#' @param ... Additional arguments passed to the `print` function.
#' @return Invisibly returns the input object.
#' @examples
#' data(climate_indices)
#' X <- climate_indices$AO
#' Y <- climate_indices$AAO
#' numberset <- c(100, 150, 200)
#' cv_results <- pcCrossValidation(X, Y, 3, 2, "euclidean", 1, FALSE, numberset = numberset)
#' print(cv_results)
#' @export
#' @method print pc_cv
print.pc_cv <- function(x, ...) {
  cat("Pattern Causality Cross-Validation Results\n")
  cat("----------------------------------------\n")
  cat("Parameters:\n")
  cat("  Embedding dimension (E):", x$parameters$E, "\n")
  cat("  Time delay (tau):", x$parameters$tau, "\n")
  cat("  Metric:", x$parameters$metric, "\n")
  cat("  Horizon (h):", x$parameters$h, "\n")
  cat("  Weighted:", x$parameters$weighted, "\n\n")
  
  cat("Sample sizes:", paste(x$samples, collapse = ", "), "\n\n")
  
  cat("Summary Statistics:\n")
  print(round(x$summary, 4))
  cat("\n")
}

#' Summary of Pattern Causality Cross Validation Results
#'
#' Provides a summary of the pattern causality cross-validation results.
#' This function calculates and displays summary statistics for the cross-validation
#' results, including sample statistics, causality statistics, and convergence.
#'
#' @param object A `pc_cv` object.
#' @param ... Additional arguments passed to the `summary` function.
#' @return Invisibly returns the input object.
#' @examples
#' data(climate_indices)
#' X <- climate_indices$AO
#' Y <- climate_indices$AAO
#' numberset <- c(100, 150, 200)
#' cv_results <- pcCrossValidation(X, Y, 3, 2, "euclidean", 1, FALSE, numberset = numberset)
#' summary(cv_results)
#' @export
#' @method summary pc_cv
summary.pc_cv <- function(object, ...) {
  structure(
    list(
      parameters = object$parameters,
      sample_stats = summary(object$samples),
      causality_stats = object$summary,
      convergence = apply(object$results, 2, function(x) {
        stats::sd(utils::tail(x, 5), na.rm = TRUE)
      })
    ),
    class = "summary.pc_cv"
  )
}

#' Plot Pattern Causality Cross Validation Results
#'
#' Visualizes the pattern causality cross-validation results.
#' This function generates a line plot showing the causality strengths
#' for different sample sizes.
#'
#' @param x A `pc_cv` object.
#' @param fr Boolean for frame display.
#' @param ... Additional arguments passed to the `plot` function.
#' @return Invisibly returns the input object.
#' @examples
#' data(climate_indices)
#' X <- climate_indices$AO
#' Y <- climate_indices$AAO
#' numbersets <- c(100, 150, 200)
#' cv_results <- pcCrossValidation(X, Y, 3, 2, "euclidean", 1, FALSE, numberset = numbersets)
#' plot(cv_results)
#' @export
#' @method plot pc_cv
plot.pc_cv <- function(x, fr = FALSE, ...) {
  plot(x$samples, x$results[,2], type = "b", pch = 19, 
       xlab = "Sample Size", ylab = "Causality Strength",
       col = "#5BA3CF", frame = fr, ylim = c(0,1))
  graphics::lines(x$samples, x$results[,3], pch = 19, col = "#F6583E", type = "b")
  graphics::lines(x$samples, x$results[,4], pch = 19, col = "#6A51A3", type = "b")
  graphics::legend("topright", c("positive", "negative", "dark"),
         col = c("#5BA3CF","#F6583E","#6A51A3"),
         pch = 19, lty = 1, bty = "n")
  invisible(x)
}

#' Pattern Causality Effect Object
#'
#' Creates a pattern causality effect object that contains information about
#' the received and exerted influences for different causality types.
#' This function constructs an object of class `pc_effect` to store the results
#' of effect analysis.
#'
#' @param positive Data frame containing positive causality effects.
#' @param negative Data frame containing negative causality effects.
#' @param dark Data frame containing dark causality effects.
#' @param items Names of items in the analysis.
#' @return An object of class "pc_effect".
#' @export
pc_effect <- function(positive = NULL, negative = NULL, dark = NULL, items = NULL) {
  # Validate input matrices have same dimensions
  if(!is.null(positive) && !is.null(negative) && !is.null(dark)) {
    if(!identical(dim(positive), dim(negative)) || 
       !identical(dim(negative), dim(dark))) {
      stop("Positive, negative and dark matrices must have identical dimensions")
    }
  }
  
  structure(
    list(
      positive = positive,
      negative = negative,
      dark = dark,
      items = items
    ),
    class = "pc_effect"
  )
}

#' Print Pattern Causality Effect
#'
#' Prints the pattern causality effect analysis results.
#' This function displays the received and exerted influences for each item
#' for positive, negative, and dark causality types.
#'
#' @param x A `pc_effect` object.
#' @param ... Additional arguments passed to the `print` function.
#' @return Invisibly returns the input object.
#' @examples
#' \donttest{
#' data(climate_indices)
#' dataset <- climate_indices[, -1]
#' pc_matrix_obj <- pcMatrix(dataset, E = 3, tau = 1, 
#'   metric = "euclidean", h = 1, weighted = TRUE, 
#'   verbose = FALSE)
#' effects <- pcEffect(pc_matrix_obj)
#' print(effects)
#' }
#' @export
#' @method print pc_effect
print.pc_effect <- function(x, ...) {
  cat("Pattern Causality Effect Analysis\n")
  cat("--------------------------------\n")
  
  types <- c("Positive", "Negative", "Dark")
  effects <- list(x$positive, x$negative, x$dark)
  
  for (i in seq_along(types)) {
    cat(sprintf("\n%s Causality Effects:\n", types[i]))
    print(round(effects[[i]], 2))
  }
  cat("\n")
}

#' Summarize Pattern Causality Effect
#'
#' Provides a summary of the pattern causality effect analysis results.
#' This function displays the summary statistics for the effects,
#' including the number of components and the strongest effects.
#'
#' @param object A `pc_effect` object.
#' @param ... Additional arguments passed to the `summary` function.
#' @return Invisibly returns the input object.
#' @examples
#' \donttest{
#' data(climate_indices)
#' dataset <- climate_indices[, -1]
#' pc_matrix_obj <- pcMatrix(dataset, E = 3, tau = 1, 
#'   metric = "euclidean", h = 1, weighted = TRUE, 
#'   verbose = FALSE)
#' effects <- pcEffect(pc_matrix_obj)
#' summary(effects)
#' }
#' @export
#' @method summary pc_effect
summary.pc_effect <- function(object, ...) {
  structure(
    list(
      effects_summary = object$summary,
      n_components = length(object$items),
      strongest_effects = lapply(
        list(object$positive, object$negative, object$dark),
        function(df) {
          idx <- which.max(abs(df$Diff))
          list(
            component = rownames(df)[idx],
            effect = df$Diff[idx]
          )
        }
      )
    ),
    class = "summary.pc_effect"
  )
}

#' Plot Pattern Causality Effect
#'
#' Generates a plot to visualize the effects of positive, negative, or dark 
#'              causality. Displays the influence exerted versus influence received for each item.
#' This function generates a scatter plot showing the influence exerted versus
#' influence received for each item, colored by the difference between exerted and received influence.
#'
#' @param x A `pc_effect` object.
#' @param status Status of the effect to plot ("positive", "negative", or "dark").
#' @param add_label Logical, whether to add labels to the plot.
#' @param point_size Numeric value for point size (default: 3).
#' @param label_size Numeric value for label text size (default: 3).
#' @param ... Additional arguments passed to plotting functions.
#' @return Invisibly returns the ggplot object.
#' @examples
#' \donttest{
#' data(climate_indices)
#' dataset <- climate_indices[, -1]
#' pc_matrix_obj <- pcMatrix(dataset, E = 3, tau = 1, 
#'   metric = "euclidean", h = 1, weighted = TRUE, 
#'   verbose = FALSE)
#' effects <- pcEffect(pc_matrix_obj)
#' plot(effects, status = "positive")
#' }
#' @export
#' @method plot pc_effect
plot.pc_effect <- function(x, status = "positive", add_label = TRUE, 
                          point_size = 3, label_size = 3, ...) {
  X <- Y <- Value <- Diff <- exerted <- received <- NULL
  if (!status %in% c("positive", "negative", "dark")) {
    stop("'status' must be one of: positive, negative, dark")
  }
  
  # Get the appropriate data frame
  data_to_plot <- switch(status,
    positive = x$positive,
    negative = x$negative,
    dark = x$dark
  )
  
  # Set color scheme based on status
  color_scheme <- switch(status,
    positive = scales::brewer_pal(palette = "Blues")(7)[-1],
    negative = scales::brewer_pal(palette = "Reds")(7)[-1],
    dark = scales::brewer_pal(palette = "Purples")(7)[-1]
  )
  
  # Create plot
  p <- ggplot2::ggplot(data_to_plot, 
                       ggplot2::aes(x = exerted, y = received)) +
    ggplot2::geom_point(ggplot2::aes(color = Diff), size = point_size) +
    ggplot2::scale_color_gradientn(
      colors = color_scheme,
      name = "(x-y)"
    ) +
    ggplot2::labs(
      x = "Influence Exerted",
      y = "Influence Received"
    ) +
    ggthemes::theme_few(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      legend.position = "right",
      axis.line.x = ggplot2::element_line(),
      axis.line.y = ggplot2::element_line(),
      legend.key = ggplot2::element_blank()
    ) +
    ggplot2::coord_fixed(ratio = 1.1)
  
  # Add labels if requested
  if (add_label) {
    p <- p + ggrepel::geom_text_repel(
      ggplot2::aes(label = rownames(data_to_plot), color = Diff),
      size = label_size,
      show.legend = FALSE
    )
  }
  
  print(p)
  invisible(p)
}

compute_matrix_stats <- function(matrix, na.rm = TRUE) {
  if(is.null(matrix)) return(NULL)
  
  list(
    mean = mean(matrix, na.rm = na.rm),
    sd = stats::sd(matrix, na.rm = na.rm),
    min = min(matrix, na.rm = na.rm),
    max = max(matrix, na.rm = na.rm)
  )
}

#' Print State Space Reconstruction
#'
#' Prints the state space reconstruction results.
#' This function displays the parameters used for state space reconstruction
#' and a preview of the reconstructed points.
#'
#' @param x A `pc_state` object.
#' @param ... Additional arguments passed to the `print` function.
#' @return Invisibly returns the input object.
#' @export
#' @method print pc_state
print.pc_state <- function(x, ...) {
  cat("State Space Reconstruction\n")
  cat("-----------------------\n")
  cat("Embedding dimension (E):", x$parameters$E, "\n")
  cat("Time delay (tau):", x$parameters$tau, "\n")
  cat("Number of points:", x$parameters$n_points, "\n")
  cat("\nFirst few reconstructed points:\n")
  if(nrow(x$matrix) > 5) {
    print(utils::head(x$matrix, 5))
    cat("... [truncated]\n")
  } else {
    print(x$matrix)
  }
  invisible(x)
}

#' Summarize State Space Reconstruction
#'
#' Provides a summary of the state space reconstruction results.
#' This function displays the dimensions, number of points, parameters,
#' summary statistics for each dimension, and the number of missing values.
#'
#' @param object A `pc_state` object.
#' @param ... Additional arguments passed to the `summary` function.
#' @return Invisibly returns the input object.
#' @export
#' @method summary pc_state
summary.pc_state <- function(object, ...) {
  structure(
    list(
      dimensions = ncol(object$matrix),
      points = nrow(object$matrix),
      parameters = object$parameters,
      matrix_stats = apply(object$matrix, 2, summary),
      missing_values = sum(is.na(object$matrix))
    ),
    class = "summary.pc_state"
  )
}

#' Plot State Space Reconstruction
#'
#' Visualizes the state space reconstruction in 3D.
#' This function generates a 3D scatter plot of the reconstructed state space.
#'
#' @param x A `pc_state` object.
#' @param style Integer; plot style (1 or 2).
#' @param verbose Logical; whether to print verbose output.
#' @param ... Additional arguments passed to the plotting functions.
#' @return Invisibly returns the input object.
#' @export
#' @method plot pc_state
plot.pc_state <- function(x, style = 2, verbose = FALSE, ...) {
  # Validate embedding dimension
  if (x$parameters$E != 3) {
    stop("3D visualization requires embedding dimension E = 3", call. = FALSE)
  }
  
  if (!(style %in% c(1, 2))) {
    stop("style must be either 1 or 2", call. = FALSE)
  }
  
  if (verbose) {
    cat("Creating state space visualization...\n")
  }
  
  # Create plot based on style
  if (style == 2) {
    plot3D::scatter3D(
      x$matrix[,1], x$matrix[,2], x$matrix[,3],
      pch = 18,
      bty = "u",
      colkey = FALSE,
      col.panel = "steelblue",
      expand = 0.4,
      col.grid = "darkblue",
      ...
    )
  } else {
    .scatter3D_fancy(
      x$matrix[,1], x$matrix[,2], x$matrix[,3],
      pch = 16,
      xlab = "",
      ylab = "",
      zlab = "",
      ticktype = "detailed",
      theta = 15,
      d = 2,
      ...
    )
  }
  
  invisible(x)
}

# Internal helper function
#' @keywords internal
#' @noRd
.scatter3D_fancy <- function(x, y, z, ..., colvar = z) {
  panelfirst <- function(pmat) {
    XY <- plot3D::trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    plot3D::scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
    
    XY <- plot3D::trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    plot3D::scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
  }
  
  plot3D::scatter3D(x, y, z, ..., colvar = colvar, panel.first = panelfirst,
            colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
}

#' @keywords internal
print.pc_prediction <- function(x, ...) {
  cat("Pattern Causality Prediction\n")
  cat("--------------------------\n")
  cat("Embedding dimension (E):", x$parameters$E, "\n")
  cat("Zero tolerance:", x$parameters$zeroTolerance, "\n\n")
  cat("Predicted signature:", x$signature, "\n")
  cat("Predicted pattern:", x$pattern, "\n")
}

#' @keywords internal
summary.pc_prediction <- function(object, ...) {
  structure(
    list(
      signature_stats = summary(object$signature),
      pattern_stats = summary(object$pattern),
      parameters = object$parameters,
      sparsity = mean(object$signature == 0)
    ),
    class = "summary.pc_prediction"
  )
}

