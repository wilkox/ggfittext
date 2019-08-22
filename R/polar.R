#' @importFrom grid makeContent
#' @export
makeContent.fittexttreepolar <- function(x) {

  data <- x$data

  # If ymin/ymax are not provided, generate boundary box from height. A similar
  # transformation will be performed for xmin/xmax for each grob individually
  # later, as it needs to be done in the context of the y position of the grob.
  if (!("ymin" %in% names(data))) {
    data$ymin <- data$r - 
      (grid::convertHeight(x$height, "npc", valueOnly = TRUE) / 2)
    data$ymax <- data$r + 
      (grid::convertHeight(x$height, "npc", valueOnly = TRUE) / 2)
  }

  # Handle parameters
  if (is.null(x$contrast)) x$contrast <- FALSE
  if (is.null(x$outside)) x$outside <- FALSE
  if (is.null(x$fullheight)) x$fullheight <- x$grow
  if (x$outside) warning("Outside is not supported in polar coordinates")
  if (x$reflow) warning("Reflowing is not supported in polar coordinates")
  if (! is.null(x$hjust)) warning("hjust is not supported in polar coordinates")
  if (! is.null(x$vjust)) warning("vjust is not supported in polar coordinates")

  # Convert padding.x and padding.y to mm
  padding.x <- grid::convertWidth(x$padding.x, "mm", valueOnly = TRUE)
  padding.y <- grid::convertHeight(x$padding.y, "mm", valueOnly = TRUE)

  # Prepare grob for each text label
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    # Convenience
    text <- data[i, ]

    # Handle angled text
    if (! text$angle == 0) warning("Angled text is not supported in polar coordinates")

    # Set hjust and vjust 
    # A vjust of 0.2 strikes a good visual balance in the kerning of characters
    # in polar coordinates
    x$hjust <- 0.5
    x$vjust <- 0.2

    # Create starting textGrob
    tg <- grid::textGrob(label = text$label, x = 0.5, y = 0.5, default.units = "mm", 
                         hjust = x$hjust, vjust = x$vjust, rot = text$angle, 
                         gp = grid::gpar(col = ggplot2::alpha(text$colour, text$alpha), 
                                         fontsize = text$size, fontfamily = text$family, 
                                         fontface = text$fontface, 
                                         lineheight = text$lineheight))

    # Get starting textGrob dimensions
    tgdim <- tgDimensions(tg, x$fullheight, text$angle)

    # Convert box y coordinates to mm
    ymin <- grid::convertHeight(grid::unit(text$ymin, "npc"), "mm", TRUE)
    ymax <- grid::convertHeight(grid::unit(text$ymax, "npc"), "mm", TRUE)

    # Get dimensions of bounding box. The y dimension will be given in mm, while
    # the x dimension is given as arc length (radians). For convenience of
    # comparing the textGrob to the bounding box on the x dimension, we will
    # also calculate it in mm based on the text placement. If xmin/xmax are
    # not provided, the boundary box will be generated from width.
    ydim <- abs(ymin - ymax) - (2 * padding.y)

    if (!("xmin" %in% names(data))) {
      if (x$place %in% c("bottomleft", "bottom", "bottomright")) {
        r <- ymin + (x$vjust * tgdim$height) + padding.y
      } else if (x$place %in% c("left", "centre", "right")) {
        r <- ((ymin + ymax) / 2) - ((0.5 - x$vjust) * tgdim$height)
      } else if (x$place %in% c("topleft", "top", "topright")) {
        r <- ymax - padding.y - ((1 - x$vjust) * tgdim$height)
      }
      c <- 2 * pi * r
      text$xmin <- text$theta - 
        (((grid::convertWidth(x$width, "mm", valueOnly = TRUE) / 2) / c) * 2 * pi)
      text$xmax <- text$theta + 
        (((grid::convertWidth(x$width, "mm", valueOnly = TRUE) / 2) / c) * 2 * pi)
    }

    xdim <- ifelse(
      text$xmax > text$xmin,
      text$xmax - text$xmin,
      (text$xmax + pi + pi - text$xmin) %% (2 * pi)
    )

    if (x$place %in% c("bottomleft", "bottom", "bottomright")) {
      r <- ymin + (x$vjust * tgdim$height) + padding.y
      xdim_mm <- r * xdim

    } else if (x$place %in% c("left", "centre", "right")) {
      r <- ((ymin + ymax) / 2) - ((0.5 - x$vjust) * tgdim$height)
      xdim_mm <- r * xdim

    } else if (x$place %in% c("topleft", "top", "topright")) {
      r <- ymax - padding.y - ((1 - x$vjust) * tgdim$height)
      xdim_mm <- r * xdim
    }

    # Resize text to fit bounding box if it doesn't fit
    if (
        # Standard condition - is text too big for box?
        (tgdim$width > xdim_mm | tgdim$height > ydim) |
          # grow = TRUE condition - is text too small for box?
          (x$grow & tgdim$width < xdim_mm & tgdim$height < ydim)
        ) {

      # Get the relationships between font size and label dimensions
      slopew <- tg$gp$fontsize / tgdim$width
      slopeh <- tg$gp$fontsize / tgdim$height

      # Calculate the target font size required to make the text fit
      # height-wise
      targetfsh <- ydim * slopeh

      # Calculate the target font size required to make the text fit width-wise
      # See https://imgur.com/a/z5TvFST for explanation of geometry
      if (x$place %in% c("bottomleft", "bottom", "bottomright")) {
        w <- xdim * (ymin + padding.y)
        targetfsw <- w * slopew

      } else if (x$place %in% c("left", "centre", "right")) {
        k <- (tgdim$height * x$vjust) / tgdim$width
        R <- (ymin + ymax) / 2
        w <- ((xdim * R) / ((xdim * k) + 1)) - (2 * padding.x)
        targetfsw <- w * slopew

      } else if (x$place %in% c("topleft", "top", "topright")) {
        k <- tgdim$height / tgdim$width
        R <- ymax - padding.y
        w <- ((xdim * R) / ((xdim * k) + 1)) - (2 * padding.x)
        targetfsw <- w * slopew
      }

      # Set to smaller of target font sizes
      tg$gp$fontsize <- ifelse(targetfsw < targetfsh, targetfsw, targetfsh)
    }

    # Hide if below minimum font size
    if (tg$gp$fontsize < x$min.size) return()

    # Update the textGrob dimensions
    tgdim <- tgDimensions(tg, x$fullheight, text$angle)

    # r = the radius from the centre to the text anchor (which is not the
    # typographic baseline but is defined by vjust). Note that the position of
    # the text anchor does not take descenders into account, so these must be
    # adjusted for if fullheight is true
    if (x$place %in% c("bottomleft", "bottom", "bottomright")) {
      r <- ymin + padding.y + (x$vjust * tgdim$height)
    } else if (x$place %in% c("left", "centre", "right")) {
      r <- ((ymin + ymax) / 2) - ((0.5 - x$vjust) * tgdim$height)
    } else if (x$place %in% c("topleft", "top", "topright")) {
      r <- ymax - padding.y - ((1 - x$vjust) * tgdim$height)
    }
    if (x$fullheight) r <- r + (grid::convertHeight(tgdim$descent, "mm", TRUE) * (1 - x$vjust))

    # c = the circumference of the baseline
    c <- 2 * pi * r

    # char_widths = widths of each character in the string
    chars <- strsplit(as.character(text$label), "")[[1]]
    char_widths <- (grid::calcStringMetric(chars)$width / 
                      sum(grid::calcStringMetric(chars)$width)) * tgdim$width

    # char_arcs = arcwidth of each character, in degrees
    char_arcs <- 360 * char_widths / c

    # padding.x.arcrad = the arcwidth of padding.x, expressed in radians, at
    # the anchor radius
    padding.x.arcrad <- (padding.x / c) * 2 * pi

    # theta = the theta of the text anchor for the entire label in the
    # coordinate system, initial calculated in radians
    if (x$place %in% c("bottomleft", "left", "topleft")) {
      theta <- text$xmin + (deg2rad(sum(char_arcs)) / 2) + padding.x.arcrad
    } else if (x$place %in% c("bottom", "centre", "top")) {
      theta <- ifelse(
        text$xmax > text$xmin,
        (text$xmin + text$xmax) / 2,
        (text$xmin + text$xmax + pi + pi) / 2
      )
    } else if (x$place %in% c("bottomright", "right", "topright")) {
      theta <- text$xmax - (deg2rad(sum(char_arcs)) / 2) - padding.x.arcrad
    }

    # angle = ?? I can't even remember what this is supposed to do but it
    # works. Converting from radians to degrees with some sort of correction?
    angle <- 450 - rad2deg(theta)

    # char_thetas = theta position of the anchors for each character (assuming
    # hjust = 0.5 for the textGrob representing this character), in degrees
    lag_vector <- function(x) c(0, x[1:length(x) - 1])
    char_thetas <- angle - lag_vector(cumsum(char_arcs)) - 
                     (char_arcs / 2) + (sum(char_arcs) / 2)

    # Generate a textGrob for each character
    tgs <- lapply(1:length(char_thetas), function(i) {

      char <- chars[i]
      theta <- char_thetas[i]
      theta_rad <- deg2rad(theta)

      x_pos <- r * cos(theta_rad)
      x_pos <- 0.5 + grid::convertWidth(grid::unit(x_pos, "mm"), "npc", TRUE)
      y_pos <- r * sin(theta_rad)
      y_pos <- 0.5 + grid::convertHeight(grid::unit(y_pos, "mm"), "npc", TRUE)

      tg <- grid::textGrob(
        label = char,
        x = x_pos,
        y = y_pos,
        hjust = x$hjust,
        vjust = x$vjust,
        rot = theta - 90,
        default.units = "npc",
        gp = grid::gpar(
          fontsize = tg$gp$fontsize,
          col = ggplot2::alpha(text$colour, text$alpha),
          fontfamily = text$family,
          fontface = text$fontface,
          lineheight = text$lineheight
        )
      )
      return(tg)
    })

    # Convert to a gTree
    gt <- grid::gTree(children = do.call(grid::gList, tgs))

    # Return the gTree
    gt
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
