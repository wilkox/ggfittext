#' @importFrom grid makeContent
#' @export
makeContent.fittexttreepolar <- function(x) {

  data <- x$data

  # Handle parameters
  if (is.null(x$contrast)) x$contrast <- FALSE
  if (is.null(x$outside)) x$outside <- FALSE
  if (x$outside) warning("Outside is not yet supported in polar coordinates")
  if (x$reflow) warning("Reflowing is not yet supported in polar coordinates")
  if (! is.null(x$hjust)) warning("hjust is not yet supported in polar coordinates")
  if (! is.null(x$vjust)) warning("vjust is not yet supported in polar coordinates")

  # Convert padding.x to mm
  padding.x <- grid::convertWidth(x$padding.x, "mm", valueOnly = TRUE)

  # Convert padding.y to npc
  padding.y <- grid::convertHeight(x$padding.y, "npc", valueOnly = TRUE)

  # Prepare grob for each text label
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    # Convenience
    text <- data[i, ]

    # Clean up angle
    text$angle <- text$angle %% 360
    if (! text$angle == 0) {
      warning("Angled text is not yet supported in polar coordinates")
    }

    # Set hjust and vjust 
    # A vjust of 0.2 strikes a good visual balance in the kerning of characters
    # in polar coordinates
    x$hjust <- 0.5
    x$vjust <- 0.2

    # If fullheight has not been set, set an appropriate value
    if (is.null(x$fullheight)) x$fullheight <- x$grow

    # Create starting textGrob
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

    # Convert box coordinates to mm
    ymin_mm <- grid::convertHeight(grid::unit(text$ymin, "npc"), "mm", TRUE)
    ymax_mm <- grid::convertHeight(grid::unit(text$ymax, "npc"), "mm", TRUE)

    # Get dimensions of bounding box
    # The y dimension will be given in mm, while the x dimension is given as
    # arc length (radians). For convenience of comparing the textGrob to the
    # bounding box on the x dimension, we will also calculate it in mm based on
    # the text placement.
    ydim <- grid::convertHeight(
      grid::unit(abs(text$ymin - text$ymax) - (2 * padding.y), "npc"),
      "mm",
      TRUE
    )
    xdim <- ifelse(
      text$xmax > text$xmin,
      text$xmax - text$xmin,
      (text$xmax + pi + pi - text$xmin) %% (2 * pi)
    )
    if (x$place %in% c("bottomleft", "bottom", "bottomright")) {
      r <- grid::convertHeight(grid::unit(text$ymin, "npc"), "mm", TRUE) +
        (x$vjust * tgdim$height) +
        grid::convertHeight(grid::unit(padding.y, "npc"), "mm", TRUE)
        xdim_mm <- r * xdim
    } else if (x$place %in% c("left", "centre", "right")) {
      r <- ((grid::convertHeight(grid::unit(text$ymin, "npc"), "mm", TRUE) +
        grid::convertHeight(grid::unit(text$ymax, "npc"), "mm", TRUE)) / 2) -
        ((0.5 - x$vjust) * tgdim$height)
    } else if (x$place %in% c("topleft", "top", "topright")) {
      r <- grid::convertHeight(grid::unit(text$ymax, "npc"), "mm", TRUE) -
        grid::convertHeight(grid::unit(padding.y, "npc"), "mm", TRUE) -
        ((1 - x$vjust) * tgdim$height)
      xdim_mm <- r * xdim
    }

    # Resize text to fit bounding box if it doesn't fit
    if (
        # Standard condition - is text too big for box?
        (tgdim$width > xdim | tgdim$height > ydim) |
          # grow = TRUE condition - is text too small for box?
          (x$grow & tgdim$width < xdim & tgdim$height < ydim)
        ) {

      # Get the relationships between font size and label dimensions
      slopew <- tg$gp$fontsize / tgdim$width
      slopeh <- tg$gp$fontsize / tgdim$height

      # Calculate the target font size required to make the text fit
      # height-wise
      targetfsh <- ydim * slopeh

      # Calculate the target font size required to make the text fit width-wise
      if (x$place %in% c("bottomleft", "bottom", "bottomright")) {
        w <- xdim * 
          (ymin_mm + grid::convertHeight(grid::unit(padding.y, "npc"), "mm", TRUE))
        targetfsw <- w * slopew

      } else if (x$place %in% c("left", "centre", "right")) {
        k <- (tgdim$height * x$vjust) / tgdim$width
        R <- (ymin_mm + ymax_mm) / 2
        w <- ((xdim * R) / ((xdim * k) + 1)) -
          (2 * padding.x)
        targetfsw <- w * slopew

      } else if (x$place %in% c("topleft", "top", "topright")) {
        k <- tgdim$height / tgdim$width
        R <- grid::convertHeight(grid::unit(text$ymax, "npc"), "mm", TRUE) -
          grid::convertHeight(grid::unit(padding.y, "npc"), "mm", TRUE)
        w <- ((xdim * R) / ((xdim * k) + 1)) -
          (2 * padding.x)
        targetfsw <- w * slopew
      }

      # Set to smaller of target font sizes
      tg$gp$fontsize <- ifelse(targetfsw < targetfsh, targetfsw, targetfsh)
    }

    # Hide if below minimum font size
    if (tg$gp$fontsize < x$min.size) return()

    # Update the textGrob dimensions
    tgdim <- tgDimensions(tg, x$fullheight, text$angle)

    # Convert the textGrob dimensions into npc
    tgdim$width <- grid::convertWidth(grid::unit(tgdim$width, "mm"), "npc", TRUE)
    tgdim$height <- grid::convertHeight(grid::unit(tgdim$height, "mm"), "npc", TRUE)

    # r = the radius from the centre to the text anchor (which is not the
    # typographic baseline but the anchor defined by vjust), initially
    # calculated in npc
    if (x$place %in% c("bottomleft", "bottom", "bottomright")) {
      r <- text$ymin + padding.y + (x$vjust * tgdim$height)
    } else if (x$place %in% c("left", "centre", "right")) {
      r <- ((text$ymin + text$ymax) / 2) - ((0.5 - x$vjust) * tgdim$height)
    } else if (x$place %in% c("topleft", "top", "topright")) {
      r <- text$ymax - padding.y - ((1 - x$vjust) * tgdim$height)
    }

    # string = the text label
    string <- as.character(text$label)

    # size = the font size of the text, in points
    size <- tg$gp$fontsize

    # Convert r to mm
    r <- grid::convertWidth(grid::unit(r, "npc"), "mm", TRUE)

    # c = the circumference of the baseline, in mm
    c <- 2 * pi * r

    # string_w = width of string, in mm
    tg <- grid::textGrob(label = string, gp = grid::gpar(fontsize = size))
    string_w <- grid::convertWidth(grid::grobWidth(tg), "mm", TRUE)

    # char_widths = width of each character in the string, in mm
    chars <- strsplit(string, "")[[1]]
    char_widths <- (grid::calcStringMetric(chars)$width / 
                      sum(grid::calcStringMetric(chars)$width)) * string_w

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
    char_thetas <- angle - dplyr::lag(cumsum(char_arcs), default = 0) - 
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
          fontsize = size,
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

