#' Default values
#'
#' @noRd
"%||%" <- function(a, b) if (is.null(a)) b else a
"%NA%" <- function(a, b) ifelse(is.na(a), b, a)

#' Conversions between degrees and radians
#'
#' @noRd
deg2rad <- function(deg) { deg * (pi / 180) }
rad2deg <- function(rad) { rad * (180 / pi) }

#' Return a reversed string
#' @param string String to be returned with reversed characters.
#' @noRd
strrev <- function(string) {
  sapply(lapply(strsplit(string, NULL), rev), paste0, collapse = "")
}

#' Textgrob dimensions, in mm
#' 
#' @noRd
tgDimensions <- function(tg, fullheight, angle) {
  width <- wunit2mm(grid::grobWidth(tg))
  height <- hunit2mm(grid::grobHeight(tg))
  if (fullheight) {
    descent <- grid::grobDescent(tg)
    width <- width + abs(wunit2mm(descent) * sin(deg2rad(angle)))
    height <- height + abs(hunit2mm(descent) * cos(deg2rad(angle)))
  } else {
    descent <- NULL
  }
  list(width = width, height = height, descent = descent)
}

#' Width and height unit conversions
#'
#' @noRd
wunit2npc <- function(w) grid::convertWidth(w, "npc", valueOnly = TRUE)
wmm2npc <- function(w) wunit2npc(grid::unit(w, "mm"))
wunit2mm <- function(w) grid::convertWidth(w, "mm", valueOnly = TRUE)
wnpc2mm <- function(w) wunit2mm(grid::unit(w, "npc"))
hunit2npc <- function(h) grid::convertHeight(h, "npc", valueOnly = TRUE)
hmm2npc <- function(h) hunit2npc(grid::unit(h, "mm"))
hunit2mm <- function(h) grid::convertHeight(h, "mm", valueOnly = TRUE)
hnpc2mm <- function(h) hunit2mm(grid::unit(h, "npc"))

#' Getters and setters for textGrob and richtext_grob
#'
#' @noRd
set_richtext_grob_param <- function(tg, param, value) {
  params <- tg$params
  params[param] <- value
  tg <- gridtext::richtext_grob(
    text = params$text,
    x = params$x,
    y = params$y,
    hjust = params$hjust,
    vjust = params$vjust,
    rot = params$rot,
    default.units = "npc",
    gp = grid::gpar(
      col = ggplot2::alpha(params$colour, params$alpha),
      fontsize = params$fontsize,
      fontfamily = params$fontfamily,
      fontface = params$fontface,
      lineheight = params$lineheight
    ),
    use_markdown = TRUE
  )
  tg$params <- params
  tg
}

getrot <- function(tg) UseMethod("getrot")
getrot.text <- function(tg) tg$rot
getrot.richtext_grob <- function(tg) tg$params$rot

getlabel <- function(tg) UseMethod("getlabel")
getlabel.text <- function(tg) tg$label
getlabel.richtext_grob <- function(tg) tg$params$text

setlabel <- function(tg, value) UseMethod("setlabel")
setlabel.text <- function(tg, value) {
  tg$label <- value
  tg
}
setlabel.richtext_grob <- function(tg, value) set_richtext_grob_param(tg, "text", value)

getfontsize <- function(tg) UseMethod("getfontsize")
getfontsize.text <- function(tg) tg$gp$fontsize
getfontsize.richtext_grob <- function(tg) tg$params$fontsize

setfontsize <- function(tg, value) UseMethod("setfontsize")
setfontsize.text <- function(tg, value) {
  tg$gp$fontsize <- value
  tg
}
setfontsize.richtext_grob <- function(tg, value) {
  set_richtext_grob_param(tg, "fontsize", value)
}

setx <- function(tg, x) UseMethod("setx")
setx.text <- function(tg, x) {
  if (!inherits(x, "unit")) x <- grid::unit(x, "npc")
  tg$x <- x
  tg
}
setx.richtext_grob <- function(tg, x) {
  set_richtext_grob_param(tg, "x", x)
}

sety <- function(tg, y) UseMethod("sety")
sety.text <- function(tg, y) {
  if (!inherits(y, "unit")) y <- grid::unit(y, "npc")
  tg$y <- y
  tg
}
sety.richtext_grob <- function(tg, y) {
  tg <- set_richtext_grob_param(tg, "y", y)
}

#' Methods to wrap labels for textGrob and richtext_grob
#'
#' These are needed because textGrob uses `\n` for line breaks while
#' richtext_grob uses `<br>`
#'
#' @noRd
wraplabel <- function(tg, w) UseMethod("wraplabel")
wraplabel.text <- function(tg, w) {
  paste(
    stringi::stri_wrap(getlabel(tg), w, normalize = FALSE),
    collapse = "\n"
  )
}
wraplabel.richtext_grob <- function(tg, w) {
  paste(
    stringi::stri_wrap(getlabel(tg), w, normalize = FALSE),
    collapse = "<br>"
  )
}
