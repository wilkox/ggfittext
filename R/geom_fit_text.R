#' A 'ggplot2' geom to fit text inside a box
#'
#' `geom_fit_text()` shrinks, grows and wraps text to fit inside a defined
#' box.
#' `geom_bar_text()` is a convenience wrapper around `geom_fit_text()` for
#' labelling bar plots generated with `geom_col()` and `geom_bar()`.
#'
#' @details
#'
#' Except where noted, `geom_fit_text()` behaves more or less like
#' `ggplot2::geom_text()`.
#'
#' There are three ways to define the box in which you want the text to be
#' drawn. The extents of the box on the x and y axes are independent, so any
#' combination of these methods can be used:
#' 
#' 1. If the `x` and/or `y` aesthetics are used to set the location of the box,
#' the width or height will be set automatically based on the number of
#' discrete values in `x` and/and `y`.
#' 2. Alternatively, if `x` and/or `y` aesthetics are used, the width and/or
#' height of the box can be overridden with a 'width' and/or 'height' argument.
#' These should be `grid::unit()` objects; if not, they will be assumed to use
#' the native axis scale.
#' 3. The boundaries of the box can be set using the aesthetics 'xmin' and
#' 'xmax', and/or 'ymin' and 'ymax'.
#'
#' If the text is too big for the box, it will be shrunk to fit the box. With
#' `grow = TRUE`, the text will be made to fill the box completely whether that
#' requires shrinking or growing.
#'
#' `reflow = TRUE` will cause the text to be reflowed (wrapped) to better fit
#' in the box. If the text cannot be made to fit by reflowing alone, it will be
#' reflowed then shrunk to fit the box.  Existing line breaks in the text will
#' be respected when reflowing.
#'
#' `geom_fit_text()` includes experimental support for drawing text in polar
#' coordinates (by adding `coord_polar()` to the plot), however not all
#' features are available when doing so.
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
#' @param padding.x,padding.y Horizontal and vertical padding around the text,
#' expressed in `grid::unit()` objects. Both default to 1 mm.
#' @param min.size Minimum font size, in points. Text that would need to be
#' shrunk below this size to fit the box will be hidden. Defaults to 4 pt (8 pt
#' for `geom_bar_text()`)
#' @param place Where inside the box to place the text. Default is 'centre';
#' other options are 'topleft', 'top', 'topright', 'right', 'bottomright',
#' 'bottom', 'bottomleft', 'left', and 'center'/'middle' which are both
#' synonyms for 'centre'. For `geom_bar_text()`, will be set heuristically if
#' not specified.
#' @param outside If `TRUE`, text placed in one of 'top', 'right', 'bottom' or
#' 'left' that would need to be shrunk smaller than `min.size` to fit the box
#' will be drawn outside the box if possible. This is mostly useful for drawing
#' text inside bar/column geoms. Defaults to TRUE for `position = "identity"`
#' when using `geom_bar_text()`, otherwise FALSE.
#' @param grow If `TRUE`, text will be grown as well as shrunk to fill the box.
#' Defaults to FALSE.
#' @param reflow If `TRUE`, text will be reflowed (wrapped) to better fit the
#' box. Defaults to FALSE.
#' @param hjust,vjust Horizontal and vertical justification of the text. By
#' default, these are automatically set to appropriate values based on `place`.
#' @param fullheight If `TRUE`, descenders will be counted when resizing and
#' placing text; if `FALSE`, only the x-height and ascenders will be counted.
#' The main use for this option is for aligning text at the baseline (`FALSE`)
#' or preventing descenders from spilling outside the box (`TRUE`). By default
#' this is set automatically depending on `place` and `grow`. 
#' @param width,height When using `x` and/or `y` aesthetics, these set the
#' width and/or height of the box. These should be either `grid::unit()`
#' objects or numeric values on the `x` and `y` scales.
#' @param formatter A function that will be applied to the text before it is
#' drawn. This is useful when using `geom_fit_text()` in context involving
#' interpolated variables, such as with the 'gganimate' package. `formatter`
#' will be applied serially to each element in the `label` column, so it does
#' not need to be a vectorised function.
#' @param contrast If `TRUE` and in combination with a `fill` aesthetic, the
#' colour of the text will be inverted for better contrast against dark
#' background fills. `FALSE` by default for `geom_fit_text()`, set
#' heuristically for `geom_bar_text()`.
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
#' @rdname geom_fit_text
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
  outside = FALSE,
  grow = FALSE,
  reflow = FALSE,
  hjust = NULL,
  vjust = NULL,
  fullheight = NULL,
  width = NULL,
  height = NULL,
  formatter = NULL,
  contrast = FALSE,
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
      outside = outside,
      min.size = min.size,
      grow = grow,
      reflow = reflow,
      hjust = hjust,
      vjust = vjust,
      fullheight = fullheight,
      width = width,
      height = height,
      formatter = formatter,
      contrast = contrast,
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
    ymax = NULL,
    fill = NULL
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
    if ((! is.null(params$width)) & (! "unit" %in% class(params$width))) {
      data$xmin <- data$x - params$width / 2
      data$xmax <- data$x + params$width / 2
    }

    # If 'height' is provided, but not a unit, interpret it as a numeric on the
    # y scale
    if ((! is.null(params$height)) & (! "unit" %in% class(params$height))) {
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

  draw_key = ggplot2::draw_key_label,

  draw_panel = function(
    data,
    panel_scales,
    coord,
    padding.x = grid::unit(1, "mm"),
    padding.y = grid::unit(1, "mm"),
    min.size = 4,
    grow = FALSE,
    reflow = FALSE,
    hjust = NULL,
    vjust = NULL,
    fullheight = NULL,
    width = NULL,
    height = NULL,
    formatter = NULL,
    contrast = FALSE,
    place = "centre",
    outside = FALSE
  ) {

    # Check if plot is in polar coordinates
    is_polar <- "CoordPolar" %in% class(coord)

    # Transform data to plot scales
    if (! is_polar) {
      data <- coord$transform(data, panel_scales)

    # For polar coordinates, we need to transform xmin/xmax & ymin/ymax into
    # theta and r values respectively; these can be used later to accurately
    # set the width and height of the bounding box in polar space
    } else if (is_polar) {
      if (is.null(data$x)) data$x <- 1
      if (is.null(data$y)) data$y <- 1
      data <- coord$transform(data, panel_scales)
      if (! is.null(data$xmin)) data$xmin <- ggplot2:::theta_rescale(coord, data$xmin, panel_scales)
      if (! is.null(data$xmax)) data$xmax <- ggplot2:::theta_rescale(coord, data$xmax, panel_scales)
      if (! is.null(data$ymin)) data$ymin <- ggplot2:::r_rescale(coord, data$ymin, panel_scales$r.range)
      if (! is.null(data$ymax)) data$ymax <- ggplot2:::r_rescale(coord, data$ymax, panel_scales$r.range)
    }

    # Reverse colours if desired
    if (contrast & "fill" %in% names(data)) {
      complement <- as.character(shades::complement(shades::shade(data$colour)))
      data$colour <- ifelse(
        shades::lightness(data$fill) < 50,
        complement,
        data$colour
      )
    }

    # Standardise the place argument
    if (place %in% c("middle", "center")) {
      place <- "centre"
    }

    gt <- grid::gTree(
      data = data,
      padding.x = padding.x,
      padding.y = padding.y,
      place = place,
      outside = outside,
      min.size = min.size,
      grow = grow,
      reflow = reflow,
      hjust = hjust,
      vjust = vjust,
      fullheight = fullheight,
      width = width,
      height = height,
      contrast = contrast,
      cl = ifelse(is_polar, "fittexttreepolar", "fittexttree")
    )
    gt$name <- grid::grobName(gt, "geom_fit_text")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.fittexttree <- function(x) {

  data <- x$data

  # Handle missing parameters
  if (is.null(x$contrast)) x$contrast <- FALSE
  if (is.null(x$outside)) x$outside <- FALSE

  # Determine which aesthetics to use for the bounding box
  # Rules: if xmin/xmax are available, use these in preference to x UNLESS
  # xmin == xmax, because this probably indicates position = "stack"; in this
  # case, use x if it is available

  # If xmin/xmax are not provided, or all xmin == xmax, generate boundary box
  # from width
  if (!("xmin" %in% names(data)) |
      (identical(data$xmin, data$xmax) & "x" %in% names(data))) {
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
      (identical(data$ymin, data$ymax) &
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

  # Remove any rows with NA boundaries
  na_rows <- which(is.na(data$xmin) | is.na(data$xmax) | is.na(data$ymin) | 
                   is.na(data$ymax))
  if (length(na_rows) > 0) {
    data <- data[-na_rows, ]
    warning(
      "Removed ",
      length(na_rows),
      " rows where box limits were outside plot limits (geom_fit_text).",
      call. = FALSE
    )
  }

  # Convert padding.x and padding.y to npc units
  padding.x <- grid::convertWidth(x$padding.x, "npc", valueOnly = TRUE)
  padding.y <- grid::convertHeight(x$padding.y, "npc", valueOnly = TRUE)

  # Prepare grob for each text label
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    # Convenience
    text <- data[i, ]

    # If the label is blank, return an empty grob
    if (text$label == "" | is.na(text$label)) {
      return(grid::nullGrob())
    }

    # Reverse colours if desired
    if (x$contrast) {
      # If contrast is set but there is no fill aesthetic, assume the default
      # ggplot2 dark grey fill
      bg_colour <- ifelse("fill" %in% names(text), text$fill, "grey35")
      text_colour <- ifelse("colour" %in% names(text), text$colour, "black")
      if (abs(shades::lightness(bg_colour) - shades::lightness(text_colour)) < 50) {
        complement <- shades::complement(shades::shade(text_colour))
        text$colour <- as.character(complement)
      }
    }

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

    # Get starting textGrob dimensions, in mm
    tgdim <- tgDimensions(tg, x$fullheight, text$angle)

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

    # The reflowing and resizing steps are encapsulated in a function to allow
    # for the 'outside' argument
    reflow_and_resize <- function(tg, x, xdim, ydim, text) {

      # Reflow the text, if reflow = TRUE and either the text doesn't currently
      # fit or grow = TRUE and if text contains spaces
      if (x$reflow & (x$grow | tgdim$width > xdim | tgdim$height > ydim) & 
          stringi::stri_detect_regex(tg$label, "\\s")) {

        # Try reducing the text width, one character at a time, and see if it
        # fits the bounding box
        best_aspect_ratio <- Inf
        best_width <- stringi::stri_length(tg$label)
        label <- unlist(stringi::stri_split(tg$label, regex = "\n"))
        stringwidth <- sum(unlist(lapply(label, stringi::stri_length)))
        previous_reflow <- ""
        for (w in (stringwidth):1) {

          # Reflow text to this width
          # By splitting the text on whitespace and passing normalize = F,
          # line breaks in the original text are respected
          tg$label <- paste(
            stringi::stri_wrap(label, w, normalize = FALSE),
            collapse = "\n"
          )

          # Skip if the text is unchanged
          if (previous_reflow == tg$label) {
            previous_reflow <- tg$label
            next
          }
          previous_reflow <- tg$label
          
          # Recalculate aspect ratio of textGrob using and update if this is the
          # new best ratio
          tgdim <- tgDimensions(tg, x$fullheight, text$angle)
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
          tgdim <- tgDimensions(tg, x$fullheight, text$angle)
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
        slopew <- tg$gp$fontsize / tgdim$width
        slopeh <- tg$gp$fontsize / tgdim$height

        # Calculate the target font size required to fit text to box along each
        # dimension
        targetfsw <- xdim * slopew
        targetfsh <- ydim * slopeh

        # Set to smaller of target font sizes
        tg$gp$fontsize <- ifelse(targetfsw < targetfsh, targetfsw, targetfsh)
      }

      # If the font size is too small and 'outside' has been set, try reflowing
      # and resizing again in the 'outside' position
      if (tg$gp$fontsize < x$min.size & x$outside) {
        if (x$place == "top") {
          text$ymin <- text$ymax
          text$ymax <- 1
          x$place <- "bottom"
        } else if (x$place == "bottom") {
          text$ymax <- text$ymin
          text$ymin <- 0
          x$place <- "top"
        } else if (x$place == "right") {
          text$xmin <- text$xmax
          text$xmax <- 1
          x$place <- "left"
        } else if (x$place == "left") {
          text$xmax <- text$xmin
          text$xmin <- 0
          x$place <- "right"
        }
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
        tg$gp$fontsize <- text$size
        x$outside <- FALSE
        # If we're moving the text outside and contrast is true, set the text
        # in contrast to the default theme_grey panel colour
        if (x$contrast) {
          bg_colour <- "grey92"
          text_colour <- ifelse("colour" %in% names(text), text$colour, "black")
          if (
            abs(shades::lightness(bg_colour) - shades::lightness(text_colour)) < 50
          ) {
            complement <- shades::complement(shades::shade(text_colour))
            tg$gp$col <- as.character(complement)
          }
        }
        return(reflow_and_resize(tg, x, xdim, ydim, text))
      }
      list(tg = tg, text = text, x = x)
    }
    fitted <- reflow_and_resize(tg, x, xdim, ydim, text)
    tg <- fitted$tg
    text <- fitted$text
    x <- fitted$x

    # Hide if below minimum font size
    if (tg$gp$fontsize < x$min.size) return()

    # Update the textGrob dimensions
    tgdim <- tgDimensions(tg, x$fullheight, text$angle)

    # To calculate the vector from the geometric centre of the text to the
    # anchor point, we first need the dimensions of the unrotated text in mm.
    # For the common use case of an orthogonal rotation, we can reuse the
    # pre-calculated values to save time
    if (tg$rot == 0 | tg$rot == 180) {
      tg_width_unrot <- tgdim$width
      tg_height_unrot <- tgdim$height
      if (x$fullheight) {
        tg_descent_unrot <- grid::convertHeight(tgdim$descent, "mm", TRUE)
      }
    } else if (tg$rot == 90 | tg$rot == 270) {
      tg_width_unrot <- tgdim$height
      tg_height_unrot <- tgdim$width
      if (x$fullheight) {
        tg_descent_unrot <- grid::convertWidth(tgdim$descent, "mm", TRUE)
      }
    } else {
      # For some
      # reason, we don't get accurate values if we do this with the original
      # textGrob so we create a copy
      unrot <- tg
      unrot$rot <- 0
      tg_width_unrot <- grid::convertWidth(grid::grobWidth(unrot), "mm", TRUE)
      tg_height_unrot <- grid::convertHeight(grid::grobHeight(unrot), "mm", TRUE)
      if (x$fullheight) {
        tg_descent_unrot <- grid::convertHeight(grid::grobDescent(unrot), "mm", TRUE)
      }
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
      grid::unit(magnitude * cos(deg2rad(direction_angle)), "mm"),
      "npc",
      TRUE
    )
    y_offset <- grid::convertHeight(
      grid::unit(magnitude * sin(deg2rad(direction_angle)), "mm"),
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

#' conversions between degrees and radians
#'
#' @noRd
deg2rad <- function(deg) { deg * (pi / 180) }
rad2deg <- function(rad) { rad * (180 / pi) }

#' Functions to get textgrob dimensions, in absolute units (mm)
#' 
#' @noRd
tgDimensions <- function(tg, fullheight, angle) {
  width <- grid::convertWidth(grid::grobWidth(tg), "mm", TRUE)
  height <- grid::convertHeight(grid::grobHeight(tg), "mm", TRUE)
  if (fullheight) {
    descent <- grid::grobDescent(tg)
    width <- width + abs(grid::convertWidth(descent, "mm", TRUE) * 
                         sin(deg2rad(angle)))
    height <- height + abs(grid::convertHeight(descent, "mm", TRUE) * 
                           cos(deg2rad(angle)))
  } else {
    descent <- NULL
  }
  list(width = width, height = height, descent = descent)
}

