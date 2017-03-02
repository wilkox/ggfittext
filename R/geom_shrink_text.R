#' @title Fit text to a bounding box.
#'
#' @description
#' \code{geom_shrink_text} will draw the label text normally, unless it is too
#' big for the bounding box, in which case it will shrink the text to fit the
#' box.
#'
#' \code{geom_fill_text} will shrink or expand the label text to fill the box.
#'
#' Except where noted, these geoms should behave like \code{geom_text}. In
#' addition to the normal \code{geom_text} aesthetics, \code{ggtextfit} geoms
#' use xmin, xmax, ymin and ymax to specify the bounding box for the label text
#' (x and y aesthetics, if passed, will be ignored).
#'
#' @param padding.x Amount of padding around text horizontally, as a grid ‘unit’
#' object. Default is 1 mm.
#' @param padding.y Amount of padding around text vertically, as a grid ‘unit’
#' object. Default is 0.1 lines.
#' @param min.size Minimum font size. If specified, text that would need to be
#' shrunk below this size to fit the bounding box will not be drawn.
#' @param place Where to place the text within the bounding box. For
#' \code{geom_shrink_text}, default is ‘centre’, other options are ‘topleft’,
#' ‘top‘, etc. For \code{geom_fill_text}, default is ‘middle’, other options are
#' ‘top’ and ‘bottom’.
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... Standard
#' geom arguments as for ‘geom_text’. Note that x and y aesthetics will be
#' ignored; xmin, xmax, ymin and ymax aesthetics specifying the bounding box are
#' required.
geom_shrink_text <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  padding.x = unit(1, "mm"),
  padding.y = unit(0.1, "lines"),
  place = "centre",
  min.size = NULL,
  ...
) {
  layer(
    geom = GeomShrinkText,
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
      place = place,
      ...
    )
  )
}

#' GeomShrinkText
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomShrinkText <- ggproto(
  "GeomShrinkText",
  Geom,
  required_aes = c("label", "xmin", "ymin", "xmax", "ymax"),
  default_aes = aes(
    alpha = 1,
    angle = 0,
    colour = "black",
    family = "",
    fontface = 1,
    lineheight = 1.2,
    size = 3.88
  ),
  draw_key = draw_key_text,
  draw_panel = function(
    data,
    panel_scales,
    coord,
    padding.x = unit(1, "mm"),
    padding.y = unit(0.1, "lines"),
    min.size = NULL,
    place = "centre"
  ) {

    data <- coord$transform(data, panel_scales)

    gt <- grid::gTree(
      data = data,
      padding.x = padding.x,
      padding.y = padding.y,
      min.size = min.size,
      place = place,
      cl = "shrinktexttree"
    )
    gt$name <- grid::grobName(gt, "geom_shrink_text")
    gt

  }
)


#' grid::makeContent function for the grobTree of shrinkTextTree objects
#' @param x A grid grobTree.
#' @export
#' @noRd
makeContent.shrinktexttree <- function(x) {

  data <- x$data

  # Padding around text
  paddingx <- grid::convertWidth(x$padding.x, "native", valueOnly = TRUE)
  paddingy <- grid::convertHeight(x$padding.y, "native", valueOnly = TRUE)

  # Prepare grob for each text label
  grobs <- lapply(1:nrow(data), function(i) {

    # Convenience
    text <- data[i, ]

    # Place text within bounding box according to 'place' aesthetic
    if (x$place == "topleft") {
      text$x <- text$xmin + paddingx
      text$y <- text$ymax - paddingy
      text$hjust <- 0
      text$vjust <- 1

    } else if (x$place == "top") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- text$ymax - paddingy
      text$hjust <- 0.5
      text$vjust <- 1

    } else if (x$place == "topright") {
      text$x <- text$xmax - paddingx
      text$y <- text$ymax - paddingy
      text$hjust <- 1
      text$vjust <- 1

    } else if (x$place == "right") {
      text$x <- text$xmax - paddingx
      text$y <- mean(c(text$ymin, text$ymax))
      text$hjust <- 1
      text$vjust <- 0.5

    } else if (x$place == "bottomright") {
      text$x <- text$xmax - paddingx
      text$y <- text$ymin + paddingy
      text$hjust <- 1
      text$vjust <- 0

    } else if (x$place == "bottom") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- text$ymin + paddingy
      text$hjust <- 0.5
      text$vjust <- 0

    } else if (x$place == "bottomleft") {
      text$x <- text$xmin + paddingx
      text$y <- text$ymin + paddingy
      text$hjust <- 0
      text$vjust <- 0

    } else if (x$place == "left") {
      text$x <- text$xmin + paddingx
      text$y <- mean(c(text$ymin, text$ymax))
      text$hjust <- 0
      text$vjust <- 0.5

    } else if (x$place == "centre" | x$place == "center") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- mean(c(text$ymin, text$ymax))
      text$hjust <- 0.5
      text$vjust <- 0.5

    } else {
      stop("geom_shrink_text does not recognise place ‘", x$place, "’ (try something like ‘topright’ or ‘centre’)", call. = F)
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

    # If the label doesn't fit, shrink it until it does
    # This is a very crude algorithm...some time later should come back and make
    # it more efficient
    while (labelw(tg) > xdim | labelh(tg) > ydim) {
      tg$gp$fontsize <- tg$gp$fontsize * 0.99
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
