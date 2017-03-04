#' @title Fit text to a bounding box.
#'
#' @details
#'
#' \code{fill.text = FALSE} (default) will draw the label text normally, unless
#' it is too big for the bounding box, in which case it will shrink the text to
#' fit the box. \code{fill.text = TRUE} will shrink or expand the label text as
#' needed to fill the box. \code{geom_shrink_text} and \code{geom_fill_text} are
#' convenience wrappers for \code{fill.text = FALSE} and \code{fill.text = TRUE}
#' respectively.
#'
#' Except where noted, these geoms should behave like \code{geom_text}. In
#' addition to the normal \code{geom_text} aesthetics, \code{ggfittext} geoms
#' use ‘xmin’, ‘xmax’, ‘ymin’ and ‘ymax’ to specify the bounding box for the
#' label text.
#'
#' If one or both axes are discrete, or for convenience, ‘x’ and/or ‘y’
#' aesthetics can be provided instead to give the centre of the bounding box.
#' The height and/or width of the boundary box will be determined by the
#' ‘discrete.height’ and/or ‘discrete.width’ aesthetics. These are given in
#' millimetres, and default to 4 mm.
#'
#' @param padding.x Amount of padding around text horizontally, as a grid ‘unit’
#' object. Default is 1 mm.
#' @param padding.y Amount of padding around text vertically, as a grid ‘unit’
#' object. Default is 0.1 lines.
#' @param min.size Minimum font size. If specified, text that would need to be
#' shrunk below this size to fit the bounding box will not be drawn.
#' @param place Where to place the text within the bounding box. Default is
#' ‘centre’, other options are ‘topleft’, ‘top’, ‘topright’, etc.
#' @param fill.text Logical, indicating whether text should expand larger than
#' the set size to fill the bounding box. Defaults to FALSE.
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... Standard
#' geom arguments as for ‘geom_text’. Note that x and y aesthetics will be
#' ignored; xmin, xmax, ymin and ymax aesthetics specifying the bounding box are
#' required.
#' @export
geom_fit_text <- function(
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
  fill.text = F,
  ...
) {
  layer(
    geom = GeomFitText,
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
      place = place,
      min.size = min.size,
      fill.text = fill.text,
      ...
    )
  )
}

#' GeomFitText
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFitText <- ggproto(
  "GeomFitText",
  Geom,
  required_aes = c("label"),
  default_aes = aes(
    alpha = 1,
    angle = 0,
    colour = "black",
    family = "",
    fontface = 1,
    lineheight = 1.2,
    size = 3.88,
    discrete.height = 4,
    discrete.width = 4
  ),
  draw_key = draw_key_text,
  draw_panel = function(
    data,
    panel_scales,
    coord,
    padding.x = unit(1, "mm"),
    padding.y = unit(0.1, "lines"),
    min.size = NULL,
    fill.text = F,
    place = "centre"
  ) {

    data <- coord$transform(data, panel_scales)

    # Check correct combination of discrete/continuous mappings for bounding box
    if (!xor(("xmin" %in% names(data)) & ("xmax" %in% names(data)), "x" %in% names(data))) {
      stop("geom_fit_text needs either ‘xmin’ and ‘xmax’, or ‘x’", .call = F)
    }
    if (!xor(("ymin" %in% names(data)) & ("ymax" %in% names(data)), "y" %in% names(data))) {
      stop("geom_fit_text needs either ‘ymin’ and ‘ymax’, or ‘y’", .call = F)
    }

    # If discrete x axis, convert x to xmin and xmax
    if ("x" %in% names(data)) {
      data$xmin <- data$x - grid::convertWidth(unit(data$discrete.height, "mm"), "native", valueOnly = T)
      data$xmax <- data$x + grid::convertWidth(unit(data$discrete.height, "mm"), "native", valueOnly = T)
    }

    # If discrete y axis, convert y to ymin and ymax
    if ("y" %in% names(data)) {
      data$ymin <- data$y - grid::convertWidth(unit(data$discrete.width, "mm"), "native", valueOnly = T)
      data$ymax <- data$y + grid::convertWidth(unit(data$discrete.width, "mm"), "native", valueOnly = T)
    }

    gt <- grid::gTree(
      data = data,
      padding.x = padding.x,
      padding.y = padding.y,
      place = place,
      min.size = min.size,
      fill.text = fill.text,
      cl = "shrinktexttree"
    )
    gt$name <- grid::grobName(gt, "geom_fit_text")
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

    } else if (x$place == "centre" | x$place == "center" | x$place == "middle") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- mean(c(text$ymin, text$ymax))
      text$hjust <- 0.5
      text$vjust <- 0.5

    } else {
      stop("geom_fit_text does not recognise place ‘", x$place, "’ (try something like ‘topright’ or ‘centre’)", call. = F)
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

    # If the label is too small and fill.text is true, expand it until it fits
    if (x$fill.text) {
      while (labelw(tg) < xdim & labelh(tg) < ydim) {
        tg$gp$fontsize <- tg$gp$fontsize * 1.01
      }
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

#' @rdname geom_fit_text
#' @export
geom_fill_text <- function(...) {
  geom_fit_text(fill.text = T, ...)
}

#' @rdname geom_fit_text
#' @export
geom_shrink_text <- function(...) {
  geom_fit_text(fill.text = F, ...)
}
