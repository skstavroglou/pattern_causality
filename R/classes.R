#' Plot Total Causality
#'
#' @param x An object
#' @param ... Additional arguments
#' @export
plot_total <- function(x, ...) {
  UseMethod("plot_total")
}

#' Plot Causality Components
#'
#' @param x An object
#' @param ... Additional arguments
#' @export
plot_components <- function(x, ...) {
  UseMethod("plot_components")
}

#' Pattern Causality Fit Object
#'
#' @description Create a pattern causality fit object
#' @param total Total causality strength
#' @param positive Positive causality strength
#' @param negative Negative causality strength
#' @param dark Dark causality strength
#' @return An object of class "pc_fit"
#' @export
pc_fit <- function(total = NULL, positive = NULL, negative = NULL, dark = NULL) {
  structure(
    list(
      total = total,
      positive = positive,
      negative = negative,
      dark = dark
    ),
    class = "pc_fit"
  )
}

#' Print Pattern Causality Results
#'
#' @param x A pc_fit object
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the input object
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
#' @param object A pc_fit object
#' @param ... Additional arguments passed to summary
#' @return Invisibly returns the input object
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
#' @param x A pc_fit object
#' @param ... Additional arguments passed to plotting functions
#' @return A ggplot object
#' @export
#' @method plot_total pc_fit
plot_total.pc_fit <- function(x, ...) {
  df1 <- data.frame(
    name = c("Non-causal", "Causal"),
    value = c(1 - x$total, x$total)
  )
  
  ggplot2::ggplot(df1, ggplot2::aes(x = name, y = value, fill = name)) +
    ggplot2::geom_bar(stat = "identity", width = 0.4) +
    ggplot2::scale_fill_manual(values = c("grey80", "grey20")) +
    ggplot2::labs(x = "", y = "Strength") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
}

#' Plot Causality Components
#'
#' @param x A pc_fit object
#' @param ... Additional arguments passed to plotting functions
#' @return A ggplot object
#' @export
#' @method plot_components pc_fit
plot_components.pc_fit <- function(x, ...) {
  df2 <- data.frame(
    name = c("Positive", "Negative", "Dark"),
    value = c(x$positive, x$negative, x$dark),
    color = c("#5BA3CF", "#F6583E", "#6A51A3")
  )
  
  ggplot2::ggplot(df2, ggplot2::aes(x = name, y = value, fill = name)) +
    ggplot2::geom_bar(stat = "identity", width = 0.4) +
    ggplot2::scale_fill_manual(values = df2$color) +
    ggplot2::labs(x = "", y = "Strength") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
}

#' Plot Pattern Causality Results
#'
#' @param x A pc_fit object
#' @param ... Additional arguments passed to plotting functions
#' @return A list of two ggplot objects
#' @export
#' @method plot pc_fit
plot.pc_fit <- function(x, ...) {
  # 创建两个图形对象
  p1 <- plot_total.pc_fit(x)
  p2 <- plot_components.pc_fit(x)
  
  # 使用 gridExtra 包的 arrangeGrob 函数来组合图形
  combined_plot <- gridExtra::arrangeGrob(p1, p2, ncol = 2)
  
  # 在新的设备上显示组合后的图形
  grid::grid.newpage()
  grid::grid.draw(combined_plot)
  
  # 返回图形对象列表
  invisible(list(total = p1, components = p2))
}
