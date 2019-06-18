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
  place = "centre",
  min.size = 4,
  grow = FALSE,
  reflow = FALSE,
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

    data

  },

  draw_key = ggplot2::draw_key_text,

  draw_panel = function(
    data,
    panel_scales,
    coord,
    padding.x = grid::unit(1, "mm"),
    padding.y = grid::unit(1, "mm"),
    min.size = 4,
    grow = FALSE,
    reflow = FALSE,
    width = grid::unit(40, "mm"),
    height = grid::unit(40, "mm"),
    formatter = NULL,
    place = "centre"
  ) {

    data <- coord$transform(data, panel_scales)

    # If a 'formatter' was provided
    if (! is.null(formatter)) {

      # Check that 'formatter' is a function
      if (! is.function(formatter)) {
        stop("`formatter` must be a function")
      }

      # Apply formatter to the labels, checking that the output is a character
      # vector of the correct length
      formatted_labels <- sapply(data$label, formatter, USE.NAMES = FALSE)
      if ((! length(formatted_labels) == length(data$label)) | 
          (! is.character(formatted_labels))) {
        stop("`formatter` must produce a character vector of same length as input")
      }
      data$label <- formatted_labels
    }

    gt <- grid::gTree(
      data = data,
      padding.x = padding.x,
      padding.y = padding.y,
      place = place,
      min.size = min.size,
      grow = grow,
      reflow = reflow,
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

    # Create textGrob
    tg <- grid::textGrob(
      label = text$label,
      x = 0.25,
      y = 0.25,
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

    # Get starting textGrob dimensions
    labelw <- function(tg) {
      grid::convertWidth(grid::grobWidth(tg), "npc", TRUE)
    }
    labelh <- function(tg) {
      tgh <- grid::convertHeight(grid::grobHeight(tg), "npc", TRUE)
      tgd <- grid::convertHeight(grid::grobDescent(tg), "npc", TRUE)
      tgh + tgd
    }
    tg_width <- labelw(tg)
    tg_height <- labelh(tg)

    # Get dimensions of bounding box
    xdim <- abs(text$xmin - text$xmax) - (2 * padding.x)
    ydim <- abs(text$ymin - text$ymax) - (2 * padding.y)

    # Get the absolute bounding box dimensions (in mm), for calculating
    # on-screen aspect ratio
    xdim_abs <- grid::convertWidth(grid::unit(xdim, "npc"), "mm", TRUE)
    ydim_abs <- grid::convertHeight(grid::unit(ydim, "npc"), "mm", TRUE)

    # Resize text to fit bounding box if it doesn't fit
    if (
      # Standard condition - is text too big for box?
      (tg_width > xdim | tg_height > ydim) |
      # grow = TRUE condition - is text too small for box?
      (x$grow & tg_width < xdim & tg_height < ydim)
    ) {

      # Reflow text if requested
      if (x$reflow) {

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
          
          # Calculate aspect ratio and update if this is the new best ratio
          tg_width <- labelw(tg)
          tg_height <- labelh(tg)
          aspect_ratio <- tg_width / tg_height
          diff_from_box_ratio <- abs(aspect_ratio - (xdim_abs / ydim_abs))
          best_diff_from_box_ratio <- abs(best_aspect_ratio - (xdim_abs / ydim_abs))
          if (diff_from_box_ratio < best_diff_from_box_ratio) {
            best_aspect_ratio <- aspect_ratio
            best_width <- w
          }

          # If the text now fits the bounding box (and we are not trying to grow
          # the text), good to stop here
          if (tg_width < xdim & tg_height < ydim & !x$grow) break
        }

        # If all reflow widths have been tried and none is smaller than the box
        # (i.e. some shrinking is still required), pick the reflow width that
        # produces the aspect ratio closest to that of the bounding box. In the
        # condition that we are trying to grow the text, this will also ensure
        # the text is grown with the best aspect ratio.
        if (tg_width > xdim | tg_height > ydim) {
          tg$label <- paste(
            stringi::stri_wrap(label, best_width, normalize = FALSE),
            collapse = "\n"
          )
          tg_width <- labelw(tg)
          tg_height <- labelh(tg)
        }
      }
      
      # Make sure there is still a reason (post-reflowing) to change the font
      # size
      if (
        # Standard condition - is text too big for box?
        (tg_width > xdim | tg_height > ydim) |
        # grow = TRUE condition - is text too small for box?
        (x$grow & tg_width < xdim & tg_height < ydim)
      ) {

        # Get the slopes of the relationships between font size and label
        # dimensions
        fs1 <- tg$gp$fontsize
        tg$gp$fontsize <- tg$gp$fontsize * 2
        slopew <- fs1 / (labelw(tg) - tg_width)
        slopeh <- fs1 / (labelh(tg) - tg_height)

        # Calculate the target font size required to fit text to box along each
        # dimension
        targetfsw <- xdim * slopew
        targetfsh <- ydim * slopeh

        # Set to smaller of target font sizes
        tg$gp$fontsize <- ifelse(targetfsw < targetfsh, targetfsw, targetfsh)
      }
    }

    # Hide if below minimum font size
    if (tg$gp$fontsize < x$min.size) { return() }

    # === Shift grob to the correct position within the bounding box

    # Specify the bounding box limits
    xmin <- text$xmin + padding.x
    xmax <- text$xmax - padding.x
    ymin <- text$ymin + padding.y
    ymax <- text$ymax - padding.y

    # Pre-calculate the dimensions of the unrotated text and adjustments We
    # need to calculate in absolute units (mm) before converting to native
    # (npc) values along each dimension to account for non-square aspect ratios
    #              +---------------+ +---------------+            
    #              |b = wcos(delta)| |c = hsin(delta)|            
    #              +---------------+ +---------------+            
    #             ^<----------------> <------------>^+-----------+
    #             |        delta     /--+           ||     d     |
    #             |          =      /   +---h       ||     =     |
    #             |       90 - rot /        +---+   ||hcos(delta)|
    #             |               /             +---v+-----------+
    #             |              /                                
    #+-----------+|             /                                 
    #|     a     ||            /                                  
    #|     =     ||           w                                   
    #|wsin(delta)||          /                                    
    #+-----------+|         /                                     
    #             |        /                                      
    #             |       /                                       
    #             |      /                                        
    #             |     /                                         
    #             |    /                                          
    #             |   /                                           
    #             |  /                                            
    #             | /                                             
    #             v/                                              
    tg$rot <- 0
    unrot_w <- grid::convertWidth(grid::grobWidth(tg), "mm", TRUE)
    unrot_h <- grid::convertHeight(grid::grobHeight(tg), "mm", TRUE) +
                 grid::convertHeight(grid::grobDescent(tg), "mm", TRUE)
    tg$rot <- text$angle
    theta <- (text$angle %% 90) * (pi / 180)
    h2npc <- function(x) grid::convertHeight(grid::unit(x, "mm"), "npc", TRUE)
    w2npc <- function(x) grid::convertWidth(grid::unit(x, "mm"), "npc", TRUE)
    a <- unrot_w * sin(theta)
    ay <- h2npc(a)
    ax <- w2npc(a)
    b <- unrot_w * cos(theta)
    by <- h2npc(b)
    bx <- w2npc(b)
    c <- unrot_h * sin(theta)
    cy <- h2npc(c)
    cx <- w2npc(c)
    d <- unrot_h * cos(theta)
    dy <- h2npc(d)
    dx <- w2npc(d)

    # Put the text in the correct horizontal justification and place,
    # correcting for rotation and the shifted anchor point
    if (x$place == "topleft") {
      tg$hjust <- 0
      tg$vjust <- 1
      if (tg$rot < 90) {
        tg_x <- xmin
        tg_y <- ymax - ay
      } else if (tg$rot < 180) {
        tg_x <- xmin + ax
        tg_y <- ymax - by - cy
      } else if (tg$rot < 270) {
        tg_x <- xmin + bx + cx
        tg_y <- ymax - dy
      } else if (tg$rot < 360) {
        tg_x <- xmin + dx
        tg_y <- ymax
      }
    } else if (x$place == "top") {
      tg$hjust <- 0.5
      tg$vjust <- 0.5 
      tg_x <- 0.5
      tg_y <- ymax - (tg_height / 2)
    } else if (x$place == "topright") {
      tg$hjust <- 1
      tg$vjust <- 1
      if (tg$rot < 90) {
        tg_x <- xmax - cx
        tg_y <- ymax
      } else if (tg$rot < 180) {
        tg_x <- xmax - ax - dx
        tg_y <- ymax - cy
      } else if (tg$rot < 270) {
        tg_x <- xmax - bx
        tg_y <- ymax - ay - dy
      } else if (tg$rot < 360) {
        tg_x <- xmax
        tg_y <- ymax - by
      }
    } else if (x$place == "right") {
      tg$hjust <- 0.5
      tg$vjust <- 0.5
      tg_x <- xmax - (tg_width / 2)
      tg_y <- 0.5
    } else if (x$place == "bottomright") {
      tg$hjust <- 1
      tg$vjust <- 0
      if (tg$rot < 90) {
        tg_x <- xmax
        tg_y <- ymin + ay
      } else if (tg$rot < 180) {
        tg_x <- xmax - ax
        tg_y <- ymin + by + cy
      } else if (tg$rot < 270) {
        tg_x <- xmax - bx - cx
        tg_y <- ymin + dy
      } else if (tg$rot < 360) {
        tg_x <- xmax - dx
        tg_y <- ymin
      }
    } else if (x$place == "bottom") {
      tg$hjust <- 0.5
      tg$vjust <- 0.5
      tg_x <- 0.5
      tg_y <- ymin + (tg_height / 2)
    } else if (x$place == "bottomleft") {
      tg$hjust <- 0
      tg$vjust <- 0
      if (tg$rot < 90) {
        tg_x <- xmin + cx
        tg_y <- ymin
      } else if (tg$rot < 180) {
        tg_x <- xmin + ax + dx
        tg_y <- ymin + cy
      } else if (tg$rot < 270) {
        tg_x <- xmin + bx
        tg_y <- ymin + ay + dy
      } else if (tg$rot < 360) {
        tg_x <- xmin
        tg_y <- ymin + by
      }
    } else if (x$place == "left") {
      tg$hjust <- 0.5
      tg$vjust <- 0.5
      tg_x <- xmin + (tg_width / 2)
      tg_y <- 0.5
    } else if (x$place %in% c("middle", "centre", "center")) {
      tg_x <- (xmin + xmax) / 2
      tg_y <- (ymin + ymax) / 2
    }

    tg$x <- grid::unit(tg_x, "npc")
    tg$y <- grid::unit(tg_y, "npc")

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
