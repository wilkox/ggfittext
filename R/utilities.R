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
#' 
#' @nord
strrev <- function(string) {
  sapply(lapply(strsplit(string, NULL), rev), paste0, collapse = "")
}

#' Textgrob dimensions, in mm
#' 
#' @noRd
tgDimensions <- function(tg, fullheight, angle) {
  width <- wunit2mm(grid::grobWidth(tg))
  height <- grid::convertHeight(grid::grobHeight(tg), "mm", TRUE)
  if (fullheight) {
    descent <- grid::grobDescent(tg)
    width <- width + abs(wunit2mm(descent) * sin(deg2rad(angle)))
    height <- height + abs(grid::convertHeight(descent, "mm", TRUE) * 
                           cos(deg2rad(angle)))
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
