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
#' reflowed then shrunk to fit the box. Existing line breaks in the text will
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
#' @param rich If `TRUE`, text will be formatted with markdown and HTML markup
#' as implemented by `gridtext::richtext_grob()`. `FALSE` by default. Rich text
#' cannot be drawn in polar coordinates. Please note that rich text support is
#' **experimental** and breaking changes are likely
#' @param data,stat,position,na.rm,show.legend,inherit.aes,... Standard geom
#' arguments as for `ggplot2::geom_text()`.
#' @param flip If `TRUE`, when in polar coordinates 'upside-down' text will be
#' flipped the 'right way up', to enhance readability.
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
  flip = FALSE,
  rich = FALSE,
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
      flip = flip,
      rich = rich,
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

  setup_params = function(data, params) {

    # Standardise the place argument
    params$place <- ifelse(params$place %in% c("middle", "center"), "centre", params$place)

    params
  },

  setup_data = function(
    data,
    params
  ) {

    # Check that valid aesthetics have been supplied for each dimension
    if (! (! is.null(data$xmin) & ! is.null(data$xmax) | ! is.null(data$x))) {
      stop(
        "geom_fit_text needs either 'xmin' and 'xmax', or 'x'",
        call. = FALSE
      )
    }
    if (! (! is.null(data$ymin) & ! is.null(data$ymax) | ! is.null(data$y))) {
      stop(
        "geom_fit_text needs either 'ymin' and 'ymax', or 'y'",
        call. = FALSE
      )
    }

    # If 'width' is provided, but not as unit, interpret it as a numeric on the
    # x scale
    if ((! is.null(params$width)) & (! inherits(params$width, "unit"))) {
      data$xmin <- data$x - params$width / 2
      data$xmax <- data$x + params$width / 2
    }

    # If 'height' is provided, but not a unit, interpret it as a numeric on the
    # y scale
    if ((! is.null(params$height)) & (! inherits(params$height, "unit"))) {
      data$ymin <- data$y - params$height / 2
      data$ymax <- data$y + params$height / 2
    }

    # If neither a 'width' parameter nor xmin/xmax aesthetics have been
    # provided, infer the width using the method of geom_boxplot
    if (is.null(params$width) & is.null(data$xmin)) {
      data$width <- ggplot2::resolution(data$x, FALSE) * 0.9
      data$xmin <- data$x - data$width / 2
      data$xmax <- data$x + data$width / 2
      data$width <- NULL
    }

    # If neither a 'height' parameter nor ymin/ymax aesthetics have been
    # provided, infer the height using the method of geom_boxplot
    if (is.null(params$height) & is.null(data$ymin)) {
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
      formatted_labels <- vapply(data$label, params$formatter, character(1), USE.NAMES = FALSE)
      if ((! length(formatted_labels) == length(data$label)) | 
          (! is.character(formatted_labels))) {
        stop("`formatter` must produce a character vector of same length as input")
      }
      data$label <- formatted_labels
    }

    data$flip <- params$flip

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
    outside = FALSE,
    rich = FALSE,
    flip = FALSE
  ) {

    # Rich text cannot be used in polar coordinates
    if (inherits(coord, "CoordPolar") & rich) {
      stop("Cannot draw rich text in polar coordinates", call. = FALSE)
    }

    # Transform data to plot scales; if in polar coordinates, we need to ensure
    # that x and y values are given
    if (inherits(coord, "CoordPolar")) {
      if (is.null(data$x)) data$x <- 1
      if (is.null(data$y)) data$y <- 1
    }
    data <- coord$transform(data, panel_scales)

    # For polar coordinates, we need to transform xmin/xmax & ymin/ymax into
    # theta and r values respectively; these can be used later to accurately
    # set the width and height of the bounding box in polar space
    if (inherits(coord, "CoordPolar")) {
      if (! is.null(data$xmin)) {
        data$xmin <- ggplot2:::theta_rescale(coord, data$xmin, panel_scales)
      }
      if (! is.null(data$xmax)) {
        data$xmax <- ggplot2:::theta_rescale(coord, data$xmax, panel_scales)
      }
      if (! is.null(data$ymin)) {
        data$ymin <- ggplot2:::r_rescale(coord, data$ymin, panel_scales$r.range)
      }
      if (! is.null(data$ymax)) {
        data$ymax <- ggplot2:::r_rescale(coord, data$ymax, panel_scales$r.range)
      }
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
      rich = rich,
      flip = flip,
      cl = ifelse(inherits(coord, "CoordPolar"), "fittexttreepolar", "fittexttree")
    )
    gt$name <- grid::grobName(gt, "geom_fit_text")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.fittexttree <- function(x) {

  # Extract data
  ftt <- x
  data <- ftt$data

  # Set and check default parameters
  ftt$outside <- ftt$outside %||% FALSE
  ftt$contrast <- ftt$contrast %||% FALSE
  ftt$vjust <- ftt$vjust %||% 0.5
  if (is.null(ftt$hjust)) {
    if (ftt$place %in% c("left", "bottomleft", "topleft")) {
      ftt$hjust <- 0
    } else if (ftt$place %in% c("right", "bottomright", "topright")) {
      ftt$hjust <- 1
    } else {
      ftt$hjust <- 0.5
    }
  }
  ftt$fullheight <- ftt$fullheight %||% ftt$grow
  ftt$rich <- ftt$rich %||% FALSE

  # Convert padding.x and padding.y to npc units
  ftt$padding.x <- wunit2npc(ftt$padding.x)
  ftt$padding.y <- hunit2npc(ftt$padding.y)

  # Default values for missing aesthetics
  data$fill <- data$fill %||% "grey35"
  data$colour <- data$colour %||% "black"

  # If xmin/xmax are not provided, generate boundary box from width
  if (is.null(data$xmin)) {
    data$xmin <- data$x - (wmm2npc(ftt$width) / 2)
    data$xmax <- data$x + (wmm2npc(ftt$width) / 2)
  }

  # If ymin/ymax are not provided, generate boundary box from height
  if (is.null(data$ymin)) {
    data$ymin <- data$y - (hmm2npc(ftt$height) / 2)
    data$ymax <- data$y + (hmm2npc(ftt$height) / 2)
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

  # Remove any rows with blank labels
  data <- data[which(! is.na(data$label) | data$label == ""), ]

  # Clean up angles
  data$angle <- data$angle %% 360

  # If contrast is set, and the shade of a text colour is too close to the
  # shade of the fill colour, change the colour to its complement
  if (ftt$contrast) {

    # If any fill value is NA, emit a warning and set to the default
    # ggplot2 background grey
    if (! is.null(data$fill)) {
      if (any(is.na(data$fill))) {
        warning("NA values in fill", call. = FALSE)
        data$fill <- data$fill %NA% "grey35"
      }
    }

    # If any colour value is NA, set to black
    data$colour <- data$colour %NA% "black"

    # Change the text colour to its complement if the background fill is too
    # dark, then change (perhaps again) if the shades of the colour and fill
    # are too similar
    data$colour <- ifelse(
      shades::lightness(data$fill) < 50,
      as.character(shades::complement(shades::shade(data$colour))),
      data$colour
    )
    data$colour <- ifelse(
      abs(shades::lightness(data$fill) - shades::lightness(data$colour)) < 50,
      as.character(shades::complement(shades::shade(data$colour))),
      data$colour
    )
  }

  # Prepare grob for each text label
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    # Convenience
    text <- data[i, ]

    # Get dimensions of bounding box, in mm
    xdim <- wnpc2mm(abs(text$xmin - text$xmax) - (2 * ftt$padding.x))
    ydim <- hnpc2mm(abs(text$ymin - text$ymax) - (2 * ftt$padding.y))

    # Reflow and/or resize the text into a textGrob
    tg <- reflow_and_resize(text, ftt$reflow, ftt$grow, ftt$fullheight, xdim, ydim, ftt$rich)

    # If the font size is too small and 'outside' has been set, try reflowing
    # and resizing again in the 'outside' position
    if (getfontsize(tg) < ftt$min.size & ftt$outside) {
      if (ftt$place == "top") {
        text$ymin <- text$ymax
        text$ymax <- 1
        ftt$place <- "bottom"
      } else if (ftt$place == "bottom") {
        text$ymax <- text$ymin
        text$ymin <- 0
        ftt$place <- "top"
      } else if (ftt$place == "right") {
        text$xmin <- text$xmax
        text$xmax <- 1
        ftt$place <- "left"
      } else if (ftt$place == "left") {
        text$xmax <- text$xmin
        text$xmin <- 0
        ftt$place <- "right"
      }
      xdim <- wnpc2mm(abs(text$xmin - text$xmax) - (2 * ftt$padding.x))
      ydim <- hnpc2mm(abs(text$ymin - text$ymax) - (2 * ftt$padding.y))
      tg <- setfontsize(tg, text$size)
      ftt$outside <- FALSE
      # If we're moving the text outside and contrast is true, set the text
      # in contrast to the default theme_grey panel colour
      if (ftt$contrast) {
        bg_colour <- "grey92"
        text_colour <- text$colour %||% "black"
        if (
          abs(shades::lightness(bg_colour) - shades::lightness(text_colour)) < 50
        ) {
          complement <- shades::complement(shades::shade(text_colour))
          text$colour <- as.character(complement)
        }
      }
      tg <- reflow_and_resize(text, ftt$reflow, ftt$grow, ftt$fullheight, xdim, ydim, ftt$rich)
    }

    # If the font size is still too small, don't draw this label
    if (getfontsize(tg) < ftt$min.size) return()

    # Set hjust and vjust
    tg$hjust <- ftt$hjust
    tg$vjust <- ftt$vjust

    # Update the textGrob dimensions
    tgdim <- tgDimensions(tg, ftt$fullheight, text$angle)

    # To calculate the vector from the geometric centre of the text to the
    # anchor point, we first need the dimensions of the unrotated text in mm.
    # For the common use case of an orthogonal rotation, we can reuse the
    # pre-calculated values to save time
    if (getrot(tg) == 0 | getrot(tg) == 180) {
      tg_width_unrot <- tgdim$width
      tg_height_unrot <- tgdim$height
      if (ftt$fullheight) tg_descent_unrot <- hunit2mm(tgdim$descent)
    } else if (getrot(tg) == 90 | getrot(tg) == 270) {
      tg_width_unrot <- tgdim$height
      tg_height_unrot <- tgdim$width
      if (ftt$fullheight) tg_descent_unrot <- wunit2mm(tgdim$descent)
    } else {
      # For some reason, we don't get accurate values if we do this with the
      # original textGrob so we create a copy
      unrot <- tg
      unrot$rot <- 0
      tg_width_unrot <- wunit2mm(grid::grobWidth(unrot))
      tg_height_unrot <- hunit2mm(grid::grobHeight(unrot))
      if (ftt$fullheight) tg_descent_unrot <- hunit2mm(grid::grobDescent(unrot))
    }

    # We can use these values to calculate the magnitude of the vector from the
    # centre point to the anchor point, using the Pythagorean identity
    if (ftt$fullheight) {
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
    direction_angle <- (direction_angle + getrot(tg)) %% 360

    # We can now use these to calculate the x and y offsets of the anchor point
    # from the centre point. For convenience, we will convert these to npc
    x_offset <- wmm2npc(magnitude * cos(deg2rad(direction_angle)))
    y_offset <- hmm2npc(magnitude * sin(deg2rad(direction_angle)))

    # Specify the bounding box limits in npc coordinates
    xmin <- text$xmin + ftt$padding.x
    xmax <- text$xmax - ftt$padding.x
    ymin <- text$ymin + ftt$padding.y
    ymax <- text$ymax - ftt$padding.y

    # Convert the textGrob dimensions into npc
    tgdim$width <- wmm2npc(tgdim$width)
    tgdim$height <- hmm2npc(tgdim$height)

    # Place the text
    if (ftt$place %in% c("topleft", "left", "bottomleft")) {
      x <- xmin + (tgdim$width / 2) + x_offset
    } else if (ftt$place %in% c("top", "centre", "bottom")) {
      x <- ((xmin + xmax) / 2) + x_offset
    } else if (ftt$place %in% c("topright", "right", "bottomright")) {
      x <- xmax - (tgdim$width / 2) + x_offset
    }
    tg <- setx(tg, x)
    if (ftt$place %in% c("topleft", "top", "topright")) {
      y <- ymax - (tgdim$height / 2) + y_offset
    } else if (ftt$place %in% c("left", "centre", "right")) {
      y <- ((ymin + ymax) / 2) + y_offset
    } else if (ftt$place %in% c("bottomleft", "bottom", "bottomright")) {
      y <- ymin + (tgdim$height / 2) + y_offset
    }
    tg <- sety(tg, y)

    # Return the textGrob
    tg
  })

  class(grobs) <- "gList"
  grid::setChildren(ftt, grobs)
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

#' Return a textGrob with the text reflowed and/or resized to fit given
#' dimensions
#'
#' @noRd
reflow_and_resize <- function(text, reflow, grow, fullheight, xdim, ydim, rich) {

  # Create textGrob or richtext_grob
  if (rich) {
    tg <- gridtext::richtext_grob(
      text = text$label,
      x = 0.5,
      y = 0.5,
      hjust = 0.5,
      vjust = 0.5,
      rot = text$angle,
      default.units = "npc",
      gp = grid::gpar(
        col = ggplot2::alpha(text$colour, text$alpha),
        fontsize = text$size,
        fontfamily = text$family,
        fontface = text$fontface,
        lineheight = text$lineheight
      ),
      use_markdown = TRUE
    )
    tg$params <- list(text = text$label, x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5, 
                      rot = text$angle, colour = text$colour, alpha = text$alpha, 
                      fontsize = text$size, fontfamily = text$family, 
                      fontface = text$fontface, lineheight = text$lineheight)

  } else {
    tg <- grid::textGrob(
      label = text$label,
      x = 0.5,
      y = 0.5,
      default.units = "npc",
      hjust = 0.5,
      vjust = 0.5,
      rot = text$angle,
      gp = grid::gpar(
        col = ggplot2::alpha(text$colour, text$alpha),
        fontsize = text$size,
        fontfamily = text$family,
        fontface = text$fontface,
        lineheight = text$lineheight
      )
    )
  }

  # Get starting textGrob dimensions, in mm
  tgdim <- tgDimensions(tg, fullheight, text$angle)

  # Reflow the text, if reflow = TRUE and either the text doesn't currently
  # fit or grow = TRUE and if text contains spaces
  if (reflow & (grow | tgdim$width > xdim | tgdim$height > ydim) & 
      stringi::stri_detect_regex(getlabel(tg), "\\s")) {

    # Generate a list of all wraps for the text
    wraps <- wraplabel(tg)

    # For each wrap, calculate the aspect ratio and return the absolute
    # difference from the box aspect ratio
    wraps$tg <- lapply(wraps$wrap, function(wrap) setlabel(tg, wrap))
    wraps$tgdim <- lapply(
      wraps$tg,
      function(tg) tgDimensions(tg, fullheight, text$angle)
    )
    wraps$aspect_ratio <- vapply(
      wraps$tgdim,
      function(tgdim) tgdim$width/tgdim$height,
      double(1)
    )
    wraps$ar_diff <- abs(wraps$aspect_ratio - (xdim / ydim))

    # If grow is false, select the widest wrap that fits the box (if there is
    # one) and return it
    if (! grow) {

      wraps$width <- unlist(lapply(wraps$tgdim, function(tgdim) tgdim$width))
      wraps$height <- unlist(lapply(wraps$tgdim, function(tgdim) tgdim$height))
      fit_wraps <- wraps[which(wraps$width < xdim), ]
      fit_wraps <- wraps[which(fit_wraps$height < ydim), ]

      if (nrow(fit_wraps) > 0) {
        best_index <- which(fit_wraps$wrapwidth == max(fit_wraps$wrapwidth))[1]
        tg <- setlabel(tg, fit_wraps$wrap[best_index])
        return(tg)
      }
    }

    # Select the wrap with the smallest difference from the box aspect ratio
    # and update the textGrob dimensions
    best_index <- which(wraps$ar_diff == min(wraps$ar_diff))[1]
    tg <- setlabel(tg, wraps$wrap[best_index])
    tgdim <- wraps$tgdim[best_index][[1]]

    # If the text now fits the bounding box (and we are not trying to grow
    # the text), good to stop here
    if (tgdim$width < xdim & tgdim$height < ydim & !grow) return(tg)
  }

  # Resize text to fit bounding box if it doesn't fit
  if (
    # Standard condition - is text too big for box?
    (tgdim$width > xdim | tgdim$height > ydim) |
    # grow = TRUE condition - is text too small for box?
    (grow & tgdim$width < xdim & tgdim$height < ydim)
  ) {

    # Get the slopes of the relationships between font size and label
    # dimensions
    slopew <- getfontsize(tg) / tgdim$width
    slopeh <- getfontsize(tg) / tgdim$height

    # Calculate the target font size required to fit text to box along each
    # dimension
    targetfsw <- xdim * slopew
    targetfsh <- ydim * slopeh

    # Set to smaller of target font sizes
    tg <- setfontsize(tg, ifelse(targetfsw < targetfsh, targetfsw, targetfsh))
  }

  return(tg)
}
