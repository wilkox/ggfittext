#' @rdname geom_shrink_text
geom_fill_text <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  padding.x = unit(1, "mm"),
  padding.y = unit(0.1, "lines"),
  min.size = NULL,
  ...
) {
  layer(
    geom = GeomFillText,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      padding.x = padding.x,
      padding.y = padding.y,
      min.size = min.size,
      ...
    )
  )
}

#' GeomFillText
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFillText <- ggproto(
  "GeomFillText",
  Geom,
  required_aes = c("label", "xmin", "ymin", "xmax", "ymax"),
  default_aes = aes(
    alpha = 1,
    angle = 0,
    colour = "black",
    family = "",
    fontface = 1,
    lineheight = 1.2,
    size = 3.88,
    place = "middle"
  ),
  draw_key = draw_key_text,
  draw_panel = function(
    data,
    panel_scales,
    coord,
    padding.x = unit(1, "mm"),
    padding.y = unit(0.1, "lines"),
    min.size = NULL
  ) {

    data <- coord$transform(data, panel_scales)

    gt <- grid::gTree(
      data = data,
      padding.x = padding.x,
      padding.y = padding.y,
      min.size = min.size,
      cl = "filltexttree"
    )
    gt$name <- grid::grobName(gt, "geom_fill_text")
    gt

  }
)


#' grid::makeContent function for the grobTree of fillTextTree objects
#' @param x A grid grobTree.
#' @export
#' @noRd
makeContent.filltexttree <- function(x) {

  data <- x$data

  # Padding around text
  paddingx <- grid::convertWidth(x$padding.x, "native", valueOnly = TRUE)
  paddingy <- grid::convertHeight(x$padding.y, "native", valueOnly = TRUE)

  # Prepare grob for each text label
  grobs <- lapply(1:nrow(data), function(i) {

    # Convenience
    text <- data[i, ]

    # Place text within bounding box according to 'place' aesthetic
    if (text$place == "top") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- text$ymax - paddingy
      text$vjust <- 1
      text$hjust <- 0.5

    } else if (text$place == "middle") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- mean(c(text$ymin, text$ymax))
      text$vjust <- 0.5
      text$hjust <- 0.5

    } else if (text$place == "bottom") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- text$ymin + paddingy
      text$vjust <- 0
      text$hjust <- 0.5
    
    } else {
      stop("geom_fill_text does not recognise place ‘", text$place, "’ (try ‘top’, ‘middle’ or ‘bottom’)", call. = F)
    }

    # Get x and y dimensions of bounding box
    xdim <- abs(text$xmin - text$xmax)
    ydim <- abs(text$ymin - text$ymax)

    # Create textGrob
    tg <- grid::textGrob(
      label = text$label,
      x = text$x,
      y = text$y,
      default.units = "native",
      hjust = text$hjust,
      vjust = text$vjust,
      rot = text$angle,
      gp = grid::gpar(
        col = alpha(text$colour, text$alpha),
        fontsize = text$size * .pt,
        fontfamily = text$family,
        fontface = text$fontface,
        lineheight = text$lineheight
      )
    )

    # Get textGrob dimensions
    labelw <- function(tg) {
      grid::convertWidth(grid::grobWidth(tg), "native", TRUE) + (2 * paddingx)
    }
    labelh <- function(tg) {
      grid::convertHeight(
        grid::grobAscent(tg) + grid::grobHeight(tg) + grid::grobDescent(tg),
        "native",
        TRUE
      ) + (2 * paddingy)
    }

    # If the label is too big, shrink it until it fits
    # This is a very crude algorithm...some time later should come back and make
    # it more efficient
    while (labelw(tg) > xdim | labelh(tg) > ydim) {
      tg$gp$fontsize <- tg$gp$fontsize * 0.99
    }

    # If the label is too small, grow it until it fits
    while (labelw(tg) < xdim & labelh(tg) < ydim) {
      tg$gp$fontsize <- tg$gp$fontsize * 1.01
    }

    # If a min size is set, don't draw if size < min size
    if (!is.null(x$min.size)) {
      if (tg$gp$fontsize < x$min.size) { return() }
    }

    # Return the textGrob
    tg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
