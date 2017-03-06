#' @title Fit text to a bounding box.
#'
#' @details
#'
#' \code{grow = FALSE} (default) will draw the label text normally, unless it is
#' too big for the bounding box, in which case it will shrink the text to fit
#' the box. \code{grow = TRUE} will shrink or grow the label text as needed to
#' fill the box. \code{geom_shrink_text} and \code{geom_grow_text} are
#' convenience wrappers for \code{grow = FALSE} and \code{grow = TRUE}
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
#' ‘height’ and/or ‘width’ aesthetics. These are given in millimetres, and
#' default to 4 mm.
#'
#' @section Aesthetics:
#'
#' \itemize{
#'   \item label (required)
#'   \item xmin
#'   \item xmax
#'   \item ymin
#'   \item ymax
#'   \item x
#'   \item y
#'   \item height (in millimetres)
#'   \item width (in millimetres)
#'   \item alpha
#'   \item angle
#'   \item colour
#'   \item family
#'   \item fontface
#'   \item lineheight
#'   \item size
#' }
#'
#' @param padding.x Amount of padding around text horizontally, as a grid ‘unit’
#' object. Default is 1 mm.
#' @param padding.y Amount of padding around text vertically, as a grid ‘unit’
#' object. Default is 0.1 lines.
#' @param min.size Minimum font size, in points. If specified, text that
#' would need to be shrunk below this size to fit the bounding box will not be
#' drawn. Defaults to 4 pt.
#' @param place Where to place the text within the bounding box. Default is
#' ‘centre’, other options are ‘topleft’, ‘top’, ‘topright’, etc.
#' @param grow Logical, indicating whether text should grow larger than
#' the set size to fill the bounding box. Defaults to FALSE.
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... Standard
#' geom arguments as for ‘geom_text’. Note that x and y aesthetics will be
#' ignored; xmin, xmax, ymin and ymax aesthetics specifying the bounding box are
#' required.
#' @param height, width (Numeric, in millimetres.) If
#' ‘xmin’/‘xmax’ and/or ‘ymin’/‘ymax’ are not provided, these values will
#' determine the dimensions of the bounding box. Default to 4 mm.
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
  min.size = 4,
  grow = F,
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
      grow = grow,
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
    size = 12,
    height = 4,
    width = 4,
    x = NULL,
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL
  ),
  draw_key = draw_key_text,
  draw_panel = function(
    data,
    panel_scales,
    coord,
    padding.x = unit(1, "mm"),
    padding.y = unit(0.1, "lines"),
    min.size = 4,
    grow = F,
    place = "centre"
  ) {

    data <- coord$transform(data, panel_scales)

    # Check correct combination of discrete/continuous mappings for bounding box
    if (!xor(
      ("xmin" %in% names(data) & "xmax" %in% names(data)),
      ("x" %in% names(data) & "width" %in% names(data))
    )) {
      stop("geom_fit_text needs either ‘xmin’ and ‘xmax’, or ‘x’ and ‘width’", .call = F)
    }
    if (!xor(
      "ymin" %in% names(data) & "ymax" %in% names(data),
      "y" %in% names(data) * "height" %in% names(data)
    )) {
      stop("geom_fit_text needs either ‘ymin’ and ‘ymax’, or ‘y’ and ‘height’", .call = F)
    }

    gt <- grid::gTree(
      data = data,
      padding.x = padding.x,
      padding.y = padding.y,
      place = place,
      min.size = min.size,
      grow = grow,
      cl = "fittexttree"
    )
    gt$name <- grid::grobName(gt, "geom_fit_text")
    gt

  }
)

#' grid::makeContent function for the grobTree of fitTextTree objects
#' @param x A grid grobTree.
#' @export
#' @noRd
makeContent.fittexttree <- function(x) {

  data <- x$data

  # If x provided instead of xmin/xmax, generate boundary box from width
  if ("x" %in% names(data)) {
    data$xmin <- data$x - (grid::convertWidth(unit(data$width, "mm"), "native", valueOnly = T) / 2)
    data$xmax <- data$x + (grid::convertWidth(unit(data$width, "mm"), "native", valueOnly = T) / 2)
  }

  # If y provided instead of ymin/ymax, generate boundary box from height
  if ("y" %in% names(data)) {
    data$ymin <- data$y - (grid::convertHeight(unit(data$height, "mm"), "native", valueOnly = T) / 2)
    data$ymax <- data$y + (grid::convertHeight(unit(data$height, "mm"), "native", valueOnly = T) / 2)
  }

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
        fontsize = text$size,
        fontfamily = text$family,
        fontface = text$fontface,
        lineheight = text$lineheight
      )
    )

    # Get textGrob dimensions
    labelw <- function(tg) {
      grid::convertWidth(grid::grobWidth(tg), "native", TRUE)
    }
    labelh <- function(tg) {
      grid::convertHeight(
        grid::grobHeight(tg) + grid::grobDescent(tg) + grid::grobAscent(tg),
        "native",
        TRUE
      )
    }

    # Get x and y dimensions of bounding box
    xdim <- abs(text$xmin - text$xmax) - (2 * paddingx)
    ydim <- abs(text$ymin - text$ymax) - (2 * paddingy)

    # Resize text to fit bounding box
    if (
      # Standard condition - is text too big for box?
      (labelw(tg) > xdim | labelh(tg) > ydim) |
      # grow = TRUE condition - is text too small for box?
      (x$grow & labelw(tg) < xdim & labelh(tg) < ydim)
    ) {

      # Get the slopes of the relationships between font size and label
      # dimensions
      fs1 <- tg$gp$fontsize
      lw1 <- labelw(tg)
      lh1 <- labelh(tg)
      tg$gp$fontsize <- tg$gp$fontsize * 2
      slopew <- fs1 / (labelw(tg) - lw1)
      slopeh <- fs1 / (labelh(tg) - lh1)

      # Calculate the target font size required to fit text to box along each
      # dimension
      targetfsw <- xdim * slopew
      targetfsh <- ydim * slopeh

      # Set to smaller of target font sizes
      tg$gp$fontsize <- ifelse(targetfsw < targetfsh, targetfsw, targetfsh)

    }

    # Hide if below minimum font size
    if (tg$gp$fontsize < x$min.size) { return() }

    # Return the textGrob
    tg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}

#' @rdname geom_fit_text
#' @export
geom_grow_text <- function(...) {
  geom_fit_text(grow = T, ...)
}

#' @rdname geom_fit_text
#' @export
geom_shrink_text <- function(...) {
  geom_fit_text(grow = F, ...)
}
