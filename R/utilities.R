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
  lapply(lapply(strsplit(string, NULL), rev), paste0, collapse = "")
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

#' @export
getrot.text <- function(tg) tg$rot

#' @export
getrot.richtext_grob <- function(tg) tg$params$rot

getlabel <- function(tg) UseMethod("getlabel")

#' @export
getlabel.text <- function(tg) tg$label

#' @export
getlabel.richtext_grob <- function(tg) tg$params$text

setlabel <- function(tg, value) UseMethod("setlabel")

#' @export
setlabel.text <- function(tg, value) {
  tg$label <- value
  tg
}

#' @export
setlabel.richtext_grob <- function(tg, value) set_richtext_grob_param(tg, "text", value)

getfontsize <- function(tg) UseMethod("getfontsize")

#' @export
getfontsize.text <- function(tg) tg$gp$fontsize

#' @export
getfontsize.richtext_grob <- function(tg) tg$params$fontsize

setfontsize <- function(tg, value) UseMethod("setfontsize")

#' @export
setfontsize.text <- function(tg, value) {
  tg$gp$fontsize <- value
  tg
}

#' @export
setfontsize.richtext_grob <- function(tg, value) {
  set_richtext_grob_param(tg, "fontsize", value)
}

setx <- function(tg, x) UseMethod("setx")

#' @export
setx.text <- function(tg, x) {
  if (!inherits(x, "unit")) x <- grid::unit(x, "npc")
  tg$x <- x
  tg
}

#' @export
setx.richtext_grob <- function(tg, x) {
  set_richtext_grob_param(tg, "x", x)
}

sety <- function(tg, y) UseMethod("sety")

#' @export
sety.text <- function(tg, y) {
  if (!inherits(y, "unit")) y <- grid::unit(y, "npc")
  tg$y <- y
  tg
}

#' @export
sety.richtext_grob <- function(tg, y) {
  tg <- set_richtext_grob_param(tg, "y", y)
}

#' Methods to wrap labels for textGrob and richtext_grob
#'
#' Each method returns a data frame containing all possible wraps for the text.
#' Newline tokens (`\n` for plain text, additionally `<br>` and its variants
#' for rich text) are respected when wrapping.
#'
#' @noRd
wraplabel <- function(tg) UseMethod("wraplabel")

#' @export
wraplabel.text <- function(tg) {

  label <- getlabel(tg)

  # Split the string into lines
  lines <- unlist(stringi::stri_split(label, regex = "\n"))

  # Identify all the line-wise locations of whitespace in the text (i.e.
  # possible widths to wrap to). Include the full lengths of each line, to
  # allow for non-wrapped wrap
  breakpoints <- lapply(
    lines,
    function(line) stringi::stri_locate_all(line, regex = "\\s")[[1]][,1]
  )
  breakpoints <- c(breakpoints, stringi::stri_length(lines))
  breakpoints <- sort(unique(unlist(breakpoints)))

  # Generate wraps for those lengths
  wraps <- data.frame(wrapwidth = breakpoints)
  wraps$wrap <- vapply(wraps$wrapwidth, function(w) {
    paste0(lapply(lines, function(line) {
      paste0(
        stringi::stri_wrap(str = line, width = w, normalise = FALSE),
        collapse = "\n"
      )
    } ), collapse = "\n")
  }, character(1))

  return(wraps)
}

#' @export
wraplabel.richtext_grob <- function(tg) {

  label <- getlabel(tg)

  # Split the string into lines
  lines <- unlist(strsplit(label, c("<br>", "</br>", "\n")))

  # Identify all the line-wise locations of whitespace in the text (i.e.
  # possible widths to wrap to). Include the full lengths of each line, to
  # allow for non-wrapped wrap
  breakpoints <- lapply(
    lines,
    function(line) stringi::stri_locate_all(line, regex = "\\s")[[1]][,1]
  )
  breakpoints <- c(breakpoints, stringi::stri_length(lines))
  breakpoints <- sort(unique(unlist(breakpoints)))

  # Generate wraps for those lengths
  wraps <- data.frame(wrapwidth = breakpoints)
  wraps$wrap <- vapply(wraps$wrapwidth, function(w) {
    paste0(lapply(lines, function(line) {
      paste0(
        wrap_rich(line, w),
        collapse = "<br>"
      )
    } ), collapse = "<br>")
  }, character(1))

  return(wraps)
}

#' An (approximate) wrapper for strings containing markdown and HTML. 
#'
#' It aims to handle (approximately) the same set of markup as gridtext.
#' Markdown parsing is implemented as a something resembling a finite state
#' machine with memoization (i.e. a packrat parser). The states are as follows:
#'
#' plain: un-marked up text
#' html_start: possibly the start of an HTML tag (i.e. a less-than sign)
#' html: an HTML tag
#' startasterisk1,startasterisk2,startasterisk3: a asterisk belonging to a
#' series of one, two or three that closes a matching pair
#' endasterisk1,endasterisk2,endasterisk3: an asterisk belonging to a series of
#' one, two or three that closes a matching pair
#' markup: other markup characters (that gridtext recognises)
#'
#' @noRd
wrap_rich <- function(string, w) {

  chars <- unlist(strsplit(string, split = NULL))

  # If the string is less than the wrap width, return the string
  if (length(chars) <= w) return(string)

  states <- character(length(chars))
  pos <- 1
  while (pos <= length(chars)) {

    # Skip if this position has already been parsed
    if (! states[pos] == "") {
      pos <- pos + 1; next
    }

    # Make sure the machine hasn't jumped ahead for some reason
    if (pos > 1) {
      if (states[pos - 1] == "") {
        cli::cli_abort("Markdown parser stuck in a bad state at position {pos} - unable to parse")
      }
    }

    # Get character
    char <- chars[pos]

    # Possibly the start of an HTML tag
    if (char == "<") {
      states[pos] <- "html_start"
      pos <- pos + 1; next
    }

    # Possibly the end of an HTML tag. Look back, and if there is an HTML start
    # tag with no intervening HTML, mark the entire tag as HTML. The states of
    # the intervening characters don't matter, a HTML tag steamrolls them all.
    if (char == ">") {
      backpos <- pos - 1
      while (backpos > 0) {
        if (states[backpos] == "html") break
        if (states[backpos] == "html_start") {
          states[seq(backpos, pos)] <- "html"
          break
        }
        backpos <- backpos - 1
      }
      if (states[pos] == "html") {
        pos <- pos + 1; next
      }
    }

    # An asterisk. Look ahead to determine if this is a one, two or
    # three-asterisk series (more than three asterisks don't count, so will begin
    # a new series). Then, look back to see if this is the end of a matched pair.
    if (char == "*") {

      if (chars[pos + 1] == "*" & ! is.na(chars[pos + 1])) {
        if (chars[pos + 2] == "*" & ! is.na(chars[pos + 2])) {
          states[seq(pos, (pos + 2))] <- "asterisk3"
        } else {
          states[seq(pos, (pos + 1))] <- "asterisk2"
        }
      } else {
        states[pos] <- "asterisk1"
      }

      backpos <- pos - 1
      while (backpos > 0) {
        if (states[pos] == "asterisk1") {
          if (states[backpos] == "endasterisk1") break
          if (states[backpos] == "asterisk1") {
            states[backpos] <- "startasterisk1"
            states[pos] <- "endasterisk1"
            break
          }
        }
        if (states[pos] == "asterisk2") {
          if (states[backpos] == "endasterisk2") break
          if (states[backpos] == "asterisk2") {
            states[seq(backpos - 1, backpos)] <- "startasterisk2"
            states[seq(pos, pos + 1)] <- "endasterisk2"
            break
          }
        }
        if (states[pos] == "asterisk3") {
          if (states[backpos] == "endasterisk3") break
          if (states[backpos] == "asterisk3") {
            states[seq(backpos - 2, backpos)] <- "startasterisk3"
            states[seq(pos, pos + 2)] <- "endasterisk3"
            break
          }
        }
        backpos <- backpos - 1
      }

      if (states[pos] == "asterisk1") pos <- pos + 1
      if (states[pos] == "asterisk2") pos <- pos + 2
      if (states[pos] == "asterisk3") pos <- pos + 3
      next
    }

    # Any other markup that gridtext knows about that does not come in a matched
    # pair
    if (char %in% c("^")) {
      states[pos] <- "markup"
      pos <- pos + 1; next
    }

    states[pos] <- "plain"
  }

  # Identify states that count as characters for the purposes of wrapping
  states <- states %in% c("plain", "asterisk1", "asterisk2", "asterisk3")

  # If the number of characters that count is less than the wrap width, return
  # the string
  if (sum(states) <= w) return(string)

  # Wrap to the specified character width
  #
  # States:
  #
  # expand: filling a line out to the desired width
  # contract: having filled to the desired width, backtracking to find somewhere
  # to insert a line break
  # hyperexpand: if there was nowhere within the width window to put a line
  # break, start looking forward to the next possible place (resulting in a
  # too-long line)
  pos <- 1
  line_l <- 0
  state <- "expand"
  while (pos <= length(chars)) {

    if (state == "expand") {
      if (line_l == w) { state <- "contract"; next }
      if (pos == length(chars)) { break }
      if (states[pos]) { line_l <- line_l + 1 }
      pos <- pos + 1
      next
    }

    if (state == "contract") {
      if (chars[pos] == " " & states[pos]) {
        chars[pos] <- "<br>"
        pos <- pos + 1
        line_l <- 0
        state <- "expand"
        next
      }
      if (chars[pos] == "<br>" | pos == 1) {
        state <- "hyperexpand"
        pos <- pos + 1
        next
      }
      pos <- pos - 1
    }

    if (state == "hyperexpand") {
      if (chars[pos] == " " & states[pos]) {
        chars[pos] <- "<br>"
        pos <- pos + 1
        line_l <- 0
        state <- "expand"
        next
      }
      if (pos == length(chars)) break
      pos <- pos + 1
      next
    }
  }
  wrapped_string <- paste(chars, collapse = "")
  return(wrapped_string)
}

theta_rescale <- function(coord, x, panel_params) {
  range <- panel_params$theta.range
  x <- pmin(pmax(x, range[1]), range[2])
  rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
  rotate(
    (x - range[1]) / diff(range) * (2 * pi)
  )
}

r_rescale <- function(coord, x, range) {
  x <- pmin(pmax(x, range[1]), range[2])
  (x - range[1]) / diff(range) * 0.4
}
