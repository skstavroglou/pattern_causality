#' @import statebins
NULL

#' Helper operator for default values
#' @noRd
`%||%` <- function(a, b) {
  if(is.null(a)) b else a
}

#' Rounded rectangle tile geom
#' @noRd
GeomRtile <- ggplot2::ggproto("GeomRtile", statebins:::GeomRrect,
  extra_params = c("na.rm"),
  
  setup_data = function(data, params) {
    data$width <- data$width %||% params$width %||% ggplot2::resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% ggplot2::resolution(data$y, FALSE)
    
    transform(data,
      xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },
  
  default_aes = ggplot2::aes(
    fill = "grey20", colour = NA, size = 0.1, linetype = 1,
    alpha = NA, width = NA, height = NA
  ),
  
  required_aes = c("x", "y"),
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
  draw_key = ggplot2::draw_key_polygon
)

#' Rounded rectangle tile layer
#' @noRd
geom_rtile <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      radius = grid::unit(6, "pt"),
                      ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRtile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      radius = radius,
      na.rm = na.rm,
      ...
    )
  )
} 