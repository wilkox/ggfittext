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
  padding = unit(1, "mm"),
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
      padding = padding,
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
    hjust = 0.5,
    lineheight = 1.2,
    size = 3.88,
    vjust = 0.5,
    place = "centre"
  ),
  draw_key = draw_key_text,
  draw_panel = function(
    data,
    panel_scales,
    coord,
    padding = unit(1, "mm"),
    min.size = NULL
  ) {

    data <- coord$transform(data, panel_scales)

    ggname("geom_shrink_text", gTree(
      data = data,
      padding = padding,
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
  paddingx <- convertWidth(x$padding, "native", valueOnly = TRUE)
  paddingy <- convertHeight(x$padding, "native", valueOnly = TRUE)

  # Prepare grob for each text label
  grobs <- lapply(1:nrow(data), function(i) {

    # Convenience
    text <- data[i, ]

    # Place text within bounding box according to 'place' aesthetic
    if (text$place == "topleft") {
      text$x <- text$xmin + paddingx
      text$y <- text$ymax - paddingy

    } else if (text$place == "top") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- text$ymax - paddingy

    } else if (text$place == "topright") {
      text$x <- text$xmax - paddingx
      text$y <- text$ymax - paddingy

    } else if (text$place == "right") {
      text$x <- text$xmax - paddingx
      text$y <- mean(c(text$ymin, text$ymax))

    } else if (text$place == "bottomright") {
      text$x <- text$xmax - paddingx
      text$y <- text$ymin + padddingy
    
    } else if (text$place == "bottom") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- text$ymin + paddingy
    
    } else if (text$place == "bottomleft") {
      text$x <- text$xmin + paddingx
      text$y <- text$ymin + paddingy

    } else if (text$place == "left") {
      text$x <- text$xmin + paddingx
      text$y <- mean(c(text$ymin, text$ymax))
    
    } else if (text$place == "centre" | text$place == "center") {
      text$x <- mean((c(text$xmin, text$xmax)))
      text$y <- mean(c(text$ymin, text$ymax))

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
    labelw <- convertWidth(grobWidth(tg), "native", TRUE) + (2 * paddingx)
    labelh <- convertHeight(grobHeight(tg), "native", TRUE) + (2 * paddingy)

    # If the label doesn't fit, shrink it until it does
    # This is a very crude algorithm...some time later should come back and make
    # it more efficient
    while (labelw > xdim | labelh > ydim) {
      tg$gp$fontsize <- tg$gp$fontsize * 0.99
      labelw <- convertWidth(grobWidth(tg), "native", TRUE) + (2 * paddingx)
      labelh <- convertHeight(grobHeight(tg), "native", TRUE) + (2 * paddingy)
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
 geom_shrink_text(hjust = 0, vjust = 1, place = "topleft", min.size = 10)
