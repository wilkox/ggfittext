#' @title A ggplot2 geom to fit text inside a rectangle
#'
#' @details
#'
#' Except where noted, these geoms should behave like \code{geom_text}. In
#' addition to the normal \code{geom_text} aesthetics, \code{ggfittext} geoms
#' use 'xmin', 'xmax', 'ymin' and 'ymax' to specify the bounding box for the
#' label text.
#'
#' If one or both axes are discrete, or for convenience, 'x' and/or 'y'
#' aesthetics can be provided instead of 'xmin', 'xmax', 'ymin' and 'ymax' to
#' give the centre of the bounding box. The height and/or width of the boundary
#' box will be determined by the 'height' and/or 'width' aesthetics. These are
#' given in millimetres, and default to 4 mm.
#'
#' \code{grow = FALSE} (default) will draw the label text normally, unless it is
#' too big for the bounding box, in which case it will shrink the text to fit
#' the box. \code{grow = TRUE} will shrink or grow the label text as needed to
#' fill the box. \code{geom_shrink_text} and \code{geom_grow_text} are
#' convenience wrappers for \code{grow = FALSE} and \code{grow = TRUE}
#' respectively.
#'
#' \code{reflow = TRUE} will cause the text to be reflowed (wrapped). When
#' \code{grow = FALSE} (default), text that doesn't fit the bounding box will be
#' reflowed until it does. If no amount of reflowing will make the text fit, the
#' reflow that best matches the aspect ratio of the bounding box will be
#' selected, and the text will then be shrunk as normal. When \code{grow =
#' TRUE}, the text will be reflowed to best match the aspect ratio of the
#' bounding box, then grown as normal. Existing line breaks ('\\n') in the text
#' will be respected when reflowing.
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
#' @param padding.x Amount of padding around text horizontally, as a grid 'unit'
#' object. Default is 1 mm.
#' @param padding.y Amount of padding around text vertically, as a grid 'unit'
#' object. Default is 0.1 lines.
#' @param min.size Minimum font size, in points. If specified, text that
#' would need to be shrunk below this size to fit the bounding box will not be
#' drawn. Defaults to 4 pt.
#' @param place Where to place the text within the bounding box. Default is
#' 'centre', other options are 'topleft', 'top', 'topright', etc.
#' @param grow Logical, indicating whether text should grow larger than
#' the set size to fill the bounding box. Defaults to FALSE. See Details.
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... Standard
#' geom arguments as for 'geom_text'. Note that x and y aesthetics will be
#' ignored; xmin, xmax, ymin and ymax aesthetics specifying the bounding box are
#' required.
#' @param height, width (Numeric, in millimetres.) If
#' 'xmin'/'xmax' and/or 'ymin'/'ymax' are not provided, these values will
#' determine the dimensions of the bounding box. Default to 4 mm.
#' @param reflow Logical, indicating whether text should be reflowed (wrapped)
#' to better fit the bounding box. See Details.
#' 
#' @export
geom_fit_text <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  padding.x = grid::unit(1, "mm"),
  padding.y = grid::unit(0.1, "lines"),
  place = "centre",
  min.size = 4,
  grow = F,
  reflow = F,
  ...
) {
  ggplot2::layer(
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
      reflow = reflow,
      ...
    )
  )
}

#' GeomFitText
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
GeomFitText <- ggplot2::ggproto(
  "GeomFitText",
  ggplot2::Geom,
  required_aes = c("label"),
  default_aes = ggplot2::aes(
    alpha = 1,
    angle = 0,
    colour = "black",
    family = "",
    fontface = 1,
    lineheight = 0.9,
    size = 12,
    height = 4,
    width = 4,
    x = NULL,
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL
  ),
  draw_key = ggplot2::draw_key_text,
  draw_panel = function(
    data,
    panel_scales,
    coord,
    padding.x = grid::unit(1, "mm"),
    padding.y = grid::unit(0.1, "lines"),
    min.size = 4,
    grow = F,
    reflow = F,
    place = "centre"
  ) {

    data <- coord$transform(data, panel_scales)

    # Check correct combination of discrete/continuous mappings for bounding box
    if (!xor(
      ("xmin" %in% names(data) & "xmax" %in% names(data)),
      ("x" %in% names(data) & "width" %in% names(data))
    )) {
      stop(
        "geom_fit_text needs either 'xmin' and 'xmax', or 'x' and 'width'",
        .call = F
      )
    }
    if (!xor(
      "ymin" %in% names(data) & "ymax" %in% names(data),
      "y" %in% names(data) * "height" %in% names(data)
    )) {
      stop(
        "geom_fit_text needs either 'ymin' and 'ymax', or 'y' and 'height'",
        .call = F
      )
    }

    gt <- grid::gTree(
      data = data,
      padding.x = padding.x,
      padding.y = padding.y,
      place = place,
      min.size = min.size,
      grow = grow,
      reflow = reflow,
      cl = "fittexttree"
    )
    gt$name <- grid::grobName(gt, "geom_fit_text")
    gt

  }
)

#' grid::makeContent function for the grobTree of fitTextTree objects
#' @param x A grid grobTree.
#' @noRd
makeContent.fittexttree <- function(x) {

  data <- x$data

  # If x provided instead of xmin/xmax, generate boundary box from width
  if ("x" %in% names(data)) {
    data$xmin <- data$x - (
      grid::convertWidth(grid::unit(data$width, "mm"), "native", valueOnly = T) / 2
    )
    data$xmax <- data$x + (
      grid::convertWidth(grid::unit(data$width, "mm"), "native", valueOnly = T) / 2
    )
  }

  # If y provided instead of ymin/ymax, generate boundary box from height
  if ("y" %in% names(data)) {
    data$ymin <- data$y - (
      grid::convertHeight(grid::unit(data$height, "mm"), "native", valueOnly = T) / 2
    )
    data$ymax <- data$y + (
      grid::convertHeight(grid::unit(data$height, "mm"), "native", valueOnly = T) / 2
    )
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
      stop(
        "geom_fit_text does not recognise place '",
        x$place,
        "' (try something like 'topright' or 'centre')",
        call. = F
      )
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

    # Hide if below minimum font size
    if (tg$gp$fontsize < x$min.size) { return() }

    # Get textGrob dimensions
    labelw <- function(tg) {
      grid::convertWidth(grid::grobWidth(tg), "native", TRUE)
    }
    labelh <- function(tg) {
      grid::convertHeight(
        grid::grobHeight(tg) + (2 * grid::grobDescent(tg)),
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

      # Reflow text if requested
      if (x$reflow) {

        # Try reducing the text width, one character at a time, and see if it
        # fits the bounding box
        best_aspect_ratio <- Inf
        best_width <- stringi::stri_length(tg$label)
        label <- unlist(stringi::stri_split(tg$label, regex = "\n"))
        stringwidth <- sum(unlist(lapply(label, stringi::stri_length)))
        for (w in (stringwidth - 1):1) {

          # Reflow text to this width
          # By splitting the text on whitespace and passing normalize = F,
          # line breaks in the original text are respected
          tg$label <- paste(
            stringi::stri_wrap(label, w, normalize = F),
            collapse = "\n"
          )

          # Calculate aspect ratio and update if this is the new best ratio
          aspect_ratio <- labelw(tg) / labelh(tg)
          diff_from_box_ratio <- abs(aspect_ratio - (xdim / ydim))
          best_diff_from_box_ratio <- abs(best_aspect_ratio - (xdim / ydim))
          if (diff_from_box_ratio < best_diff_from_box_ratio) {
            best_aspect_ratio <- aspect_ratio
            best_width <- w
          }

          # If the text now fits the bounding box (and we are not trying to grow
          # the text), good to stop and return the grob
          if (labelw(tg) < xdim & labelh(tg) < ydim & !x$grow) {
            return(tg)
          }
        }

        # If all reflow widths have been tried and none is smaller than the box
        # (i.e. some shrinking is still required), pick the reflow width that
        # produces the aspect ratio closest to that of the bounding box. In the
        # condition that we are trying to grow the text, this will also ensure
        # the text is grown with the best aspect ratio.
        tg$label <- paste(
          stringi::stri_wrap(label, best_width, normalize = F),
          collapse = "\n"
        )
      }

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
