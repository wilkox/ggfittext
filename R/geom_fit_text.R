#' A 'ggplot2' geom to fit text inside a box
#'
#' `geom_fit_text()` shrinks, grows or wraps text to fit inside a defined
#' rectangular area.
#'
#' @details
#'
#' Except where noted, `geom_fit_text()` behaves more or less like
#' `ggplot2::geom_text()`.
#'
#' There are three ways to define the box in which you want the text to be
#' drawn:
#' 
#' 1. On a continuous axis, the limits of the box can be defined in the data
#' using plot aesthetics: 'xmin' and 'xmax', and/or 'ymin' and 'ymax'.
#' 2. Alternatively on a continuous axis, the centre of the box can be defined
#' in the data with the 'x' and/or 'y' aesthetic, and the width and/or height
#' of the box can be specified with a 'width' and/or 'height' argument. 'width'
#' and 'height' should be provided as `grid::unit()` objects; if not, they will
#' be assumed to use the native axis scale.
#' 3. On a discrete (categorical) axis, the width or height will be determined
#' automatically. This can be overridden if you wish using the 'width' and
#' 'height' arguments.
#'
#' By default, the text will be drawn as if with `geom_text()`, unless it is
#' too big for the box, in which case it will be shrunk to fit the box. With
#' `grow = TRUE`, the text will be made to fill the box completely whether
#' that requires shrinking or growing.
#'
#' `reflow = TRUE` will cause the text to be reflowed (wrapped) to better fit
#' in the box. When `grow = FALSE` (default), text that fits the box will be
#' drawn as if with `geom_text()`; text that doesn't fit the box will be
#' reflowed until it does. If the text cannot be made to fit by reflowing
#' alone, it will be reflowed to match the aspect ratio of the box as closely
#' as possible, then be shrunk to fit the box. When `grow = TRUE`, the text
#' will be reflowed to best match the aspect ratio of the box, then made to
#' fill the box completely whether that requires growing or shrinking. Existing
#' line breaks in the text will be respected when reflowing.
#'
#' @section Aesthetics:
#'
#' - label (required)
#' - (xmin AND xmax) OR x (required)
#' - (ymin AND ymax) OR y (required)
#' - alpha
#' - angle
#' - colour
#' - family
#' - fontface
#' - lineheight
#' - size
#'
#' @param padding.x,padding.y Amount of horizontal and vertical padding around
#' the text, expressed as `grid::unit()` objects. Both default to 1 mm.
#' @param min.size Minimum font size, in points. If provided, text that would
#' need to be shrunk below this size to fit the box will not be drawn. Defaults
#' to 4 pt.
#' @param place Where inside the box to place the text. Default is 'centre';
#' other options are 'topleft', 'top', 'topright', etc.
#' @param grow If `TRUE`, text will be grown as well as shrunk to fill the box.
#' See Details.
#' @param reflow If `TRUE`, text will be reflowed (wrapped) to better fit the
#' box. See Details.
#' @param hjust,vjust Horizontal and vertical justification of text. By
#' default, these are automatically set to appropriate values based on `place`
#' and `angle`.
#' @param width,height When using `x` and/or `y` aesthetics, these can be used
#' to set the width and/or height of the box. These should be either
#' numeric values on the `x` and `y` scales or `grid::unit()` objects.
#' @param formatter A function that will be applied to the text before it is
#' drawn. This can be useful when using `geom_fit_text()` in an automated
#' context, such as with the 'gganimate' package. `formatter` will be applied
#' serially to each element in the `label` column, so it does not need to be a
#' vectorised function.
#' @param mapping `ggplot2::aes()` object as standard in 'ggplot2'. Note
#' that aesthetics specifying the box must be provided. See Details.
#' @param data,stat,position,na.rm,show.legend,inherit.aes,... Standard geom
#' arguments as for `ggplot2::geom_text()`.
#'
#' @examples
#'
#' ggplot2::ggplot(ggplot2::presidential, ggplot2::aes(ymin = start, ymax = end,
#'     label = name, x = party)) +
#'   geom_fit_text(grow = TRUE)
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
  padding.y = grid::unit(1, "mm"),
  min.size = 4,
  place = "centre",
  grow = FALSE,
  reflow = FALSE,
  hjust = NULL,
  vjust = NULL,
  fullheight = NULL,
  width = NULL,
  height = NULL,
  formatter = NULL,
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
      hjust = hjust,
      vjust = vjust,
      fullheight = fullheight,
      width = width,
      height = height,
      formatter = formatter,
      ...
    )
  )
}

#' GeomFitText
#' @noRd
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
    x = NULL,
    y = NULL,
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL
  ),

  setup_data = function(
    data,
    params
  ) {

    # Check that valid aesthetics have been supplied for each dimension
    if (!(
      ("xmin" %in% names(data) & "xmax" %in% names(data)) |
      ("x" %in% names(data))
    )) {
      stop(
        "geom_fit_text needs either 'xmin' and 'xmax', or 'x'",
        .call = FALSE
      )
    }
    if (!(
      "ymin" %in% names(data) & "ymax" %in% names(data) |
      "y" %in% names(data)
    )) {
      stop(
        "geom_fit_text needs either 'ymin' and 'ymax', or 'y'",
        .call = FALSE
      )
    }

    # If 'width' is provided, but not as unit, interpret it as a numeric on the
    # x scale
    if ((! is.null(params$width)) & class(params$width) != "unit") {
      data$xmin <- data$x - params$width / 2
      data$xmax <- data$x + params$width / 2
    }

    # If 'height' is provided, but not a unit, interpret it as a numeric on the
    # y scale
    if ((! is.null(params$height)) & class(params$height) != "unit") {
      data$ymin <- data$y - params$height / 2
      data$ymax <- data$y + params$height / 2
    }

    # If neither a 'width' parameter nor xmin/xmax aesthetics have been
    # provided, infer the width using the method of geom_boxplot
    if (is.null(params$width) & ! "xmin" %in% names(data)) {
      data$width <- ggplot2::resolution(data$x, FALSE) * 0.9
      data$xmin <- data$x - data$width / 2
      data$xmax <- data$x + data$width / 2
      data$width <- NULL
    }

    # If neither a 'height' parameter nor ymin/ymax aesthetics have been
    # provided, infer the height using the method of geom_boxplot
    if (is.null(params$height) & ! "ymin" %in% names(data)) {
      data$height <- ggplot2::resolution(data$y, FALSE) * 0.9
      data$ymin <- data$y - data$height / 2
      data$ymax <- data$y + data$height / 2
      data$height <- NULL
    }

    # Apply a formatter function, if one was given
    if (! is.null(params$formatter)) {

      # Check that 'formatter' is a function
      if (! is.function(params$formatter)) {
        stop("`formatter` must be a function")
      }

      # Apply formatter to the labels, checking that the output is a character
      # vector of the correct length
      formatted_labels <- sapply(data$label, params$formatter, USE.NAMES = FALSE)
      if ((! length(formatted_labels) == length(data$label)) | 
          (! is.character(formatted_labels))) {
        stop("`formatter` must produce a character vector of same length as input")
      }
      data$label <- formatted_labels
    }

    data
  },

  draw_key = ggplot2::draw_key_text,

  draw_panel = function(
    data,
    panel_scales,
    coord,
    padding.x = grid::unit(1, "mm"),
    padding.y = grid::unit(2, "mm"),
    min.size = 4,
    grow = FALSE,
    reflow = FALSE,
    hjust = NULL,
    vjust = NULL,
    fullheight = NULL,
    width = NULL,
    height = NULL,
    formatter = NULL,
    place = "centre"
  ) {

    data <- coord$transform(data, panel_scales)

    gt <- grid::gTree(
      data = data,
      padding.x = padding.x,
      padding.y = padding.y,
      place = place,
      min.size = min.size,
      grow = grow,
      reflow = reflow,
      hjust = hjust,
      vjust = vjust,
      fullheight = fullheight,
      width = width,
      height = height,
      cl = "fittexttree"
    )
    gt$name <- grid::grobName(gt, "geom_fit_text")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.fittexttree <- function(x) {

  data <- x$data

  # Standardise the place argument
  if (x$place %in% c("middle", "center")) {
    x$place <- "centre"
  }

  # Determine which aesthetics to use for the bounding box
  # Rules: if xmin/xmax are available, use these in preference to x UNLESS
  # xmin == xmax, because this probably indicates position = "stack"; in this
  # case, use x if it is available

  # If xmin/xmax are not provided, or all xmin == xmax, generate boundary box
  # from width
  if (!("xmin" %in% names(data)) | 
      (all(data$xmin == data$xmax) & "x" %in% names(data))) {
    data$xmin <- data$x - (
      grid::convertWidth(
        grid::unit(x$width, "mm"),
        "npc",
        valueOnly = TRUE
      ) / 2
    )
    data$xmax <- data$x + (
      grid::convertWidth(
        grid::unit(x$width, "mm"),
        "npc",
        valueOnly = TRUE
      ) / 2
    )
  }

  # If ymin/ymax are not provided, or all ymin == ymax, generate boundary box
  # from height
  if (!("ymin" %in% names(data)) | 
      (all(data$ymin == data$ymax) & 
       "y" %in% names(data))) {
    data$ymin <- data$y - (
      grid::convertHeight(
        grid::unit(x$height, "mm"),
        "npc",
        valueOnly = TRUE
      ) / 2
    )
    data$ymax <- data$y + (
      grid::convertHeight(
        grid::unit(x$height, "mm"),
        "npc",
        valueOnly = TRUE
      ) / 2
    )
  }

  # Convert padding.x and padding.y to npc units
  padding.x <- grid::convertWidth(x$padding.x, "npc", valueOnly = TRUE)
  padding.y <- grid::convertHeight(x$padding.y, "npc", valueOnly = TRUE)

  # Prepare grob for each text label
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    # Convenience
    text <- data[i, ]

    # Clean up angle
    text$angle <- text$angle %% 360

    # If hjust and/or vjust have not been set, set an appropriate value based
    # on place
    if (is.null(x$hjust)) {
      if (x$place %in% c("left", "bottomleft", "topleft")) {
        x$hjust <- 0
      } else if (x$place %in% c("right", "bottomright", "topright")) {
        x$hjust <- 1
      } else {
        x$hjust <- 0.5
      }
    }
    if (is.null(x$vjust)) {
      x$vjust <- 0.5
    }

    # If fullheight has not been set, set an appropriate value
    if (is.null(x$fullheight)) {
      x$fullheight <- x$grow
    }

    # Create textGrob
    tg <- grid::textGrob(
      label = text$label,
      x = 0.5,
      y = 0.5,
      default.units = "npc",
      hjust = x$hjust,
      vjust = x$vjust,
      rot = text$angle,
      gp = grid::gpar(
        col = ggplot2::alpha(text$colour, text$alpha),
        fontsize = text$size,
        fontfamily = text$family,
        fontface = text$fontface,
        lineheight = text$lineheight
      )
    )

    # Functions to get textgrob dimensions, in absolute units (mm)
    tgDimensions <- function(tg) {
      width <- grid::convertWidth(grid::grobWidth(tg), "mm", TRUE)
      height <- grid::convertHeight(grid::grobHeight(tg), "mm", TRUE)
      if (x$fullheight) {
        descent <- grid::grobDescent(tg)
        width <- width + abs(grid::convertWidth(descent, "mm", TRUE) * 
                             sin(text$angle * (pi / 180)))
        height <- height + abs(grid::convertHeight(descent, "mm", TRUE) * 
                               cos(text$angle * (pi / 180)))
      }
      list(width = width, height = height)
    }

    # Get starting textGrob dimensions, in mm
    tgdim <- tgDimensions(tg)

    # Get dimensions of bounding box, in mm
    xdim <- grid::convertWidth(
      grid::unit(abs(text$xmin - text$xmax) - (2 * padding.x), "npc"),
      "mm",
      TRUE
    )
    ydim <- grid::convertHeight(
      grid::unit(abs(text$ymin - text$ymax) - (2 * padding.y), "npc"),
      "mm",
      TRUE
    )

    # Reflow the text, if reflow = TRUE and either the text doesn't currently
    # fit or grow = TRUE
    if (x$reflow & (x$grow | tgdim$width > xdim | tgdim$height > ydim)) {

      # Try reducing the text width, one character at a time, and see if it
      # fits the bounding box
      best_aspect_ratio <- Inf
      best_width <- stringi::stri_length(tg$label)
      label <- unlist(stringi::stri_split(tg$label, regex = "\n"))
      stringwidth <- sum(unlist(lapply(label, stringi::stri_length)))
      for (w in (stringwidth):1) {

        # Reflow text to this width
        # By splitting the text on whitespace and passing normalize = F,
        # line breaks in the original text are respected
        tg$label <- paste(
          stringi::stri_wrap(label, w, normalize = FALSE),
          collapse = "\n"
        )
        
        # Recalculate aspect ratio of textGrob using and update if this is the
        # new best ratio
        tgdim <- tgDimensions(tg)
        aspect_ratio <- tgdim$width / tgdim$height
        diff_from_box_ratio <- abs(aspect_ratio - (xdim / ydim))
        best_diff_from_box_ratio <- abs(best_aspect_ratio - (xdim / ydim))
        if (diff_from_box_ratio < best_diff_from_box_ratio) {
          best_aspect_ratio <- aspect_ratio
          best_width <- w
        }

        # If the text now fits the bounding box (and we are not trying to grow
        # the text), good to stop here
        if (tgdim$width < xdim & tgdim$height < ydim & !x$grow) break
      }

      # If all reflow widths have been tried and none is smaller than the box
      # (i.e. some shrinking is still required), or if we are trying to grow
      # the text, pick the reflow width that produces the aspect ratio closest
      # to that of the bounding box
      if (tgdim$width > xdim | tgdim$height > ydim | x$grow) {
        tg$label <- paste(
          stringi::stri_wrap(label, best_width, normalize = FALSE),
          collapse = "\n"
        )
        # Update the textGrob dimensions
        tgdim <- tgDimensions(tg)
      }

    }

    # Resize text to fit bounding box if it doesn't fit
    if (
      # Standard condition - is text too big for box?
      (tgdim$width > xdim | tgdim$height > ydim) |
      # grow = TRUE condition - is text too small for box?
      (x$grow & tgdim$width < xdim & tgdim$height < ydim)
    ) {

      # Get the slopes of the relationships between font size and label
      # dimensions
      fs1 <- tg$gp$fontsize
      tg$gp$fontsize <- tg$gp$fontsize * 2
      new_dimensions <- tgDimensions(tg)
      slopew <- fs1 / (new_dimensions$width - tgdim$width)
      slopeh <- fs1 / (new_dimensions$height - tgdim$height)

      # Calculate the target font size required to fit text to box along each
      # dimension
      targetfsw <- xdim * slopew
      targetfsh <- ydim * slopeh

      # Set to smaller of target font sizes
      tg$gp$fontsize <- ifelse(targetfsw < targetfsh, targetfsw, targetfsh)

      # Hide if below minimum font size
      if (tg$gp$fontsize < x$min.size) { return() }

      # Update the textGrob dimensions. We can save a little time here by
      # cheating and using the known relationships between font size and
      # dimensions, rather than recalculating
      tgdim$width <- tg$gp$fontsize / slopew
      tgdim$height <- tg$gp$fontsize / slopeh
    }

    # === Calculate vector from geometric centre of text to anchor point

    # First, we need the dimensions of the unrotated text in mm. For some
    # reason, we don't get accurate values if we do this with the original
    # textGrob so we create a copy
    unrot <- tg
    unrot$rot <- 0
    tg_width_unrot <- grid::convertWidth(grid::grobWidth(unrot), "mm", TRUE)
    tg_height_unrot <- grid::convertHeight(grid::grobHeight(unrot), "mm", TRUE)
    if (x$fullheight) {
      tg_descent_unrot <- grid::convertHeight(grid::grobDescent(unrot), "mm", TRUE)
    }

    # We can use these values to calculate the magnitude of the vector from the
    # centre point to the anchor point, using the Pythagorean identity
    if (x$fullheight) {
      rise <- ((tg_height_unrot * tg$vjust) + tg_descent_unrot) - 
                ((tg_height_unrot + tg_descent_unrot) * 0.5)
    } else {
      rise <- (tg_height_unrot * tg$vjust) - (tg_height_unrot * 0.5)
    }
    run <- (tg_width_unrot * tg$hjust) - (tg_width_unrot * 0.5)
    magnitude <- sqrt((rise ^ 2) + (run ^ 2))

    # The angle between the baseline and the vector can be calculated from the
    # known rise and run
    baseline_angle <- asin(abs(rise) / abs(magnitude)) * (180 / pi)

    # To find the 'direction angle' of the vector (expressed in degrees
    # anti-clockwise from east), we correct for the quadrant then add the
    # rotation of the entire textGrob modulo 360. There's almost certainly a
    # clever trigonometry way to do this but I can't figure it out
    if (sign(rise) == 1 & sign(run) == 1) {
      direction_angle <- baseline_angle
    } else if (sign(rise) == 1 & sign(run) == 0) {
      direction_angle <- 90
    } else if (sign(rise) == 1 & sign(run) == -1) {
      direction_angle <- 180 - baseline_angle
    } else if (sign(rise) == 0 & sign(run) == -1) {
      direction_angle <- 180
    } else if (sign(rise) == -1 & sign(run) == -1) {
      direction_angle <- 180 + baseline_angle
    } else if (sign(rise) == -1 & sign(run) == 0) {
      direction_angle <- 270
    } else if (sign(rise) == -1 & sign(run) == 1) {
      direction_angle <- 360 - baseline_angle
    } else if (sign(rise) == 0 & sign(run) == 1) {
      direction_angle <- 0
    } else if (sign(rise) == 0 & sign(run) == 0) {
      direction_angle <- 0
    }
    direction_angle <- (direction_angle + tg$rot) %% 360

    # We can now use these to calculate the x and y offsets of the anchor point
    # from the centre point. For convenience, we will convert these to npc
    x_offset <- grid::convertWidth(
      grid::unit(magnitude * cos(direction_angle * (pi / 180)), "mm"),
      "npc",
      TRUE
    )
    y_offset <- grid::convertHeight(
      grid::unit(magnitude * sin(direction_angle * (pi / 180)), "mm"),
      "npc",
      TRUE
    )

    # Specify the bounding box limits in npc coordinates
    xmin <- text$xmin + padding.x
    xmax <- text$xmax - padding.x
    ymin <- text$ymin + padding.y
    ymax <- text$ymax - padding.y

    # Convert the textGrob dimensions into npc
    tgdim$width <- grid::convertWidth(grid::unit(tgdim$width, "mm"), "npc", TRUE)
    tgdim$height <- grid::convertHeight(grid::unit(tgdim$height, "mm"), "npc", TRUE)

    # Place the text
    if (x$place %in% c("topleft", "left", "bottomleft")) {
      tg$x <- xmin + (tgdim$width / 2) + x_offset
    } else if (x$place %in% c("top", "centre", "bottom")) {
      tg$x <- ((xmin + xmax) / 2) + x_offset
    } else if (x$place %in% c("topright", "right", "bottomright")) {
      tg$x <- xmax - (tgdim$width / 2) + x_offset
    }
    if (x$place %in% c("topleft", "top", "topright")) {
      tg$y <- ymax - (tgdim$height / 2) + y_offset
    } else if (x$place %in% c("left", "centre", "right")) {
      tg$y <- ((ymin + ymax) / 2) + y_offset
    } else if (x$place %in% c("bottomleft", "bottom", "bottomright")) {
      tg$y <- ymin + (tgdim$height / 2) + y_offset
    }

    # Convert x and y coordinates to unit objects
    tg$x <- grid::unit(tg$x, "npc")
    tg$y <- grid::unit(tg$y, "npc")

    # Return the textGrob
    tg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}

#' geom_fit_text
#' @noRd
geom_grow_text <- function(...) {
  .Deprecated("geom_fit_text(grow = T, ...)")
}

#' geom_fit_text
#' @noRd
geom_shrink_text <- function(...) {
  .Deprecated("geom_fit_text(grow = F, ...)")
}
