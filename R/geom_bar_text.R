#' @rdname geom_fit_text
#'
#' @export
geom_bar_text <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  padding.x = grid::unit(1, "mm"),
  padding.y = grid::unit(1, "mm"),
  min.size = 8,
  place = "top",
  grow = FALSE,
  reflow = FALSE,
  hjust = NULL,
  vjust = NULL,
  fullheight = NULL,
  width = NULL,
  height = NULL,
  formatter = NULL,
  contrast = FALSE,
  outside = TRUE,
  ...
) {
  ggplot2::layer(
    geom = GeomBarText,
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
      contrast = contrast,
      outside = outside,
      ...
    )
  )
}

GeomBarText <- ggplot2::ggproto(
  "GeomBarText",
  ggplot2::Geom,
  required_aes = c("x", "y"),
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

    # If the label is missing, assume y is the label (as with stat_count)
    if (! "label" %in% names(data)) {
      data$label <- data$y
    }

    # Set xmin and xmax using the method of geom_boxplot
    width <- ggplot2::resolution(data$x, FALSE) * 0.9
    data$xmin <- data$x - width / 2
    data$xmax <- data$x + width / 2

    # Set starting ymin and ymax
    data$ymin <- 0
    data$ymax <- data$y

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
    padding.y = grid::unit(1, "mm"),
    min.size = 8,
    grow = FALSE,
    reflow = FALSE,
    hjust = NULL,
    vjust = NULL,
    fullheight = NULL,
    width = NULL,
    height = NULL,
    formatter = NULL,
    contrast = FALSE,
    place = "top",
    outside = TRUE
  ) {

    # Split data into negative and positive y values
    positives <- subset(data, data$y >= 0)
    negatives <- subset(data, data$y < 0)

    # Draw positives with "place" as given
    gtrees <- list()
    if (nrow(positives) > 0) {
      positives <- coord$transform(positives, panel_scales)
      positives_gt <- grid::gTree(
        data = positives,
        padding.x = padding.x,
        padding.y = padding.y,
        place = place,
        outside = outside,
        min.size = min.size,
        grow = grow,
        reflow = reflow,
        contrast = contrast,
        cl = "fittexttree"
      )
      gtrees$positives <- positives_gt
    }

    # Draw negatives with reversed place
    if (nrow(negatives) > 0) {
      negatives <- coord$transform(negatives, panel_scales)
      if (place == "top") {
        place <- "bottom"
        negatives$ymax <- negatives$ymin
        negatives$ymin <- negatives$y
      } else if (place == "right") {
        place <- "left"
        negatives$xmax <- negatives$xmin
        negatives$xmin <- negatives$x
      } else if (place == "bottom") {
        place <- "top"
        negatives$ymax <- negatives$ymin
        negatives$ymin <- negatives$y
      } else if (place == "left") {
        place <- "right"
        negatives$xmax <- negatives$xmin
        negatives$xmin <- negatives$x
      }
      negatives_gt <- grid::gTree(
        data = negatives,
        padding.x = padding.x,
        padding.y = padding.y,
        place = place,
        outside = outside,
        min.size = min.size,
        grow = grow,
        reflow = reflow,
        contrast = contrast,
        cl = "fittexttree"
      )
      gtrees$negatives <- negatives_gt
    }

    gt <- grid::gTree(children = do.call(grid::gList, gtrees))
    gt$name <- grid::grobName(gt, "geom_bar_text")
    gt
  }
)