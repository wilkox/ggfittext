# TODO stolen directly from slowkow/ggrepel
# Name ggplot grid object
# Convenience function to name grid objects
#
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

#' Shrink text to fit box
#' TODO write some more documentation
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
    size = 3.88,
    place = "centre"
  ),
  draw_key = draw_key_text,
  draw_panel = function(
    data,
    panel_scales,
    coord,
    padding.x = unit(1, "mm"),
    padding.y = unit(0.1, "lines"),
    min.size = NULL
  ) {

    data <- coord$transform(data, panel_scales)

    ggname("geom_shrink_text", gTree(
      data = data,
      padding.x = padding.x,
      padding.y = padding.y,
      min.size = min.size,
      cl = "shrinktexttree"
    ))

  }
)


#' grid::makeContent function for the grobTree of shrinkTextTree objects
#' @param x A grid grobTree.
#' @export
#' @noRd
makeContent.shrinktexttree <- function(x) {

  data <- x$data

  # Padding around text
  paddingx <- convertWidth(x$padding.x, "native", valueOnly = TRUE)
  paddingy <- convertHeight(x$padding.y, "native", valueOnly = TRUE)

  # Prepare grob for each text label
  grobs <- lapply(1:nrow(data), function(i) {

    # Convenience
    text <- data[i, ]

    # Place text within bounding box according to 'place' aesthetic
    if (text$place == "topleft") {
      text$x <- text$xmin + paddingx
      text$y <- text$ymax - paddingy
      text$hjust <- 0
      text$vjust <- 1

    } else if (text$place == "top") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- text$ymax - paddingy
      text$hjust <- 0.5
      text$vjust <- 1

    } else if (text$place == "topright") {
      text$x <- text$xmax - paddingx
      text$y <- text$ymax - paddingy
      text$hjust <- 1
      text$vjust <- 1

    } else if (text$place == "right") {
      text$x <- text$xmax - paddingx
      text$y <- mean(c(text$ymin, text$ymax))
      text$hjust <- 1
      text$vjust <- 0.5

    } else if (text$place == "bottomright") {
      text$x <- text$xmax - paddingx
      text$y <- text$ymin + paddingy
      text$hjust <- 1
      text$vjust <- 0

    } else if (text$place == "bottom") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- text$ymin + paddingy
      text$hjust <- 0.5
      text$vjust <- 0

    } else if (text$place == "bottomleft") {
      text$x <- text$xmin + paddingx
      text$y <- text$ymin + paddingy
      text$hjust <- 0
      text$vjust <- 0

    } else if (text$place == "left") {
      text$x <- text$xmin + paddingx
      text$y <- mean(c(text$ymin, text$ymax))
      text$hjust <- 0
      text$vjust <- 0.5

    } else if (text$place == "centre" | text$place == "center") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- mean(c(text$ymin, text$ymax))
      text$hjust <- 0.5
      text$vjust <- 0.5

    } else {
      stop("geom_shrink_text does not recognise place ‘", text$place, "’ (try something like ‘topright’ or ‘centre’)", call. = F)
    }

    # Get x and y dimensions of bounding box
    xdim <- abs(text$xmin - text$xmax)
    ydim <- abs(text$ymin - text$ymax)

    # Create textGrob
    tg <- textGrob(
      label = text$label,
      x = text$x,
      y = text$y,
      default.units = "native",
      hjust = text$hjust,
      vjust = text$vjust,
      rot = text$angle,
      gp = gpar(
        col = alpha(text$colour, text$alpha),
        fontsize = text$size * .pt,
        fontfamily = text$family,
        fontface = text$fontface,
        lineheight = text$lineheight
      )
    )

    # Get textGrob dimensions
    labelw <- function(tg) {
      convertWidth(grobWidth(tg), "native", TRUE) + (2 * paddingx)
    }
    labelh <- function(tg) {
      convertHeight(
        grobAscent(tg) + grobHeight(tg) + grobDescent(tg),
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
  setChildren(x, grobs)
}

treeMapCoordinates <- treemapify(
  G20,
  area = "Nom.GDP.mil.USD",
  fill = "HDI",
  label = "Country",
  group = "Region"
)
treeMapCoordinates %>% 
  ggplot(aes(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    label = label,
    fill = fill
  )) + 
 geom_rect() + 
 geom_shrink_text(place = "centre", min.size = 6)
