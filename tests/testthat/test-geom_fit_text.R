library(ggplot2)
library(vdiffr)

test_that("geom_fit_text() places text correctly", {
  
  expect_doppelganger("Default placement (middle)", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text()
  } )

  expect_doppelganger("Placing at the top", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(place = "top")
  } )

  expect_doppelganger("Placing at the bottom right", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(place = "bottomright")
  } )

} )

test_that("geom_fit_text() grows and reflows text correctly", {

  expect_doppelganger("Growing without reflowing", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(grow = TRUE)
  } )

  expect_doppelganger("Growing with placement at the bottom", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(grow = TRUE, place = "bottom")
  } )

  expect_doppelganger("Growing with reflowing", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(grow = TRUE, reflow = TRUE)
  } )

  expect_doppelganger("Reflowing without growing", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(reflow = TRUE)
  } )

  animals2 <- animals
  animals2$animal[1] <- paste(
    "Whose woods these are I think I know.",
    "His house is in the village though;",
    "He will not see me stopping here",
    "To watch his woods fill up with snow.",
    sep = "\n"
  )
  animals2$animal[2]<- paste(
    "Whose woods these are I think I know.",
    "His house is in the village though;",
    "He will not see me stopping here",
    "To watch his woods fill up with snow.",
    sep = " "
  )

  expect_doppelganger("Complex text with reflowing and growing", {
    ggplot(animals2, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(grow = TRUE, reflow = TRUE)
  } )

  expect_doppelganger("Complex text with reflowing only", {
    ggplot(animals2, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(reflow = TRUE)
  } )

} )

test_that("The width and height arguments work correctly", {

  expect_doppelganger("Numeric width and height", {
    df <- data.frame(
      vehicle = c("light plane", "jumbo jet", "space shuttle"),
      x = c(15, 40, 85),
      y = c(15, 85, 23.5)
    )
    ggplot(df, aes(x = x, y = y, label = vehicle)) + 
      geom_tile(width = 10, height = 20, fill = "gray") + 
      geom_fit_text(width = 10, height = 20)
  } )

} )

test_that("The formatter argument works correctly", {

  expect_doppelganger("Label with formatter function", {
    wikif <- function(x) { paste0(x, " (citation needed)") }
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(grow = TRUE, reflow = TRUE, formatter = wikif)
  } )

} )

test_that("geom_fit_text() handles blank labels", {

  expect_doppelganger("Blank label", {
    presidential$name[1] <- ""
    ggplot(presidential, aes(ymin = start, ymax = end, label = name, x = party)) +
      geom_fit_text(grow = TRUE)
  } )

} )

test_that("geom_fit_text() warns when box limits are outside of plot limits", {

  df <- data.frame(x = letters[1:5], y = 0:4, lb = 3)

  expect_warning( {
    p <- ggplot(df, aes(x = x, y = y, label = lb)) + 
      geom_fit_text() + 
      ylim(0.5, 3)
    print(p)
  }, "box limits were outside plot limits")

  expect_warning( {
    p <- ggplot(df, aes(x = x, y = y, label = lb)) + 
      ylim(-0.1, 6) + 
      geom_fit_text()
    print(p)
  }, "box limits were outside plot limits")

  expect_warning( {
    p <- ggplot(df, aes(x = y, y = x, label = lb)) + 
      xlim(-0.1, 6) + 
      geom_fit_text()
    print(p)
  }, "box limits were outside plot limits")

} )

test_that("geom_fit_text() draws angled text correctly", {

  expect_doppelganger("Angled text", {
    ggplot(animals, aes(x = type, y = flies, label = animal, angle = mass)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(size = 48)
  } )

} )

test_that("geom_fit_text() draws in boxes defined with xmin/xmax and ymin/ymax", {

  expect_doppelganger("Box defined with x and ymin/ymax", {
    ggplot(presidential, aes(ymin = start, ymax = end, x = party, label = name)) +
      geom_fit_text(grow = TRUE, size = 48) +
      geom_errorbar(alpha = 0.5)
  } )

  expect_doppelganger("Box defined with xmin/xmax and y", {
    ggplot(presidential, aes(xmin = start, xmax = end, y = party, label = name)) +
      geom_fit_text(grow = TRUE, size = 48) +
      geom_errorbarh(alpha = 0.5)
  } )

  expect_doppelganger("Box defined with xmin/xmax and ymin/ymax", {
    ggplot(presidential, aes(xmin = start, xmax = end, ymin = start, ymax = end, 
                             label = name)) +
      geom_fit_text(grow = TRUE, size = 48)
  } )

} )

test_that("geom_fit_text() draws contrasting text", {

  expect_doppelganger("Contrasting text", {
    ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +
      geom_tile() +
      geom_fit_text(reflow = TRUE, grow = TRUE, contrast = TRUE, size = 48)
  } )

  expect_doppelganger("Contrasting with non-black text", {
    ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +
      geom_tile() +
      geom_fit_text(colour = "thistle", reflow = TRUE, grow = TRUE, 
                    contrast = TRUE) +
       scale_fill_gradientn(
         colours = c("red","yellow","green","lightblue","darkblue"),
         values = c(1.0,0.8,0.6,0.4,0.2,0)
       ) 
  } )

} )

test_that("geom_fit_text() draws, grows and correctly places text in polar coordinates", {

  expect_doppelganger("Polar geom_fit_text() plot", {
    ggplot(gold, aes(label = line, xmin = xmin, xmax = xmax, ymin = ymin, 
                     ymax = ymax)) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(min.size = 0)
  } )

  expect_doppelganger("Polar plot with growing, non-fullheight", {
    ggplot(gold, aes(label = line, xmin = xmin, xmax = xmax, ymin = ymin, 
                     ymax = ymax)) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(grow = TRUE, fullheight = FALSE, min.size = 0)
  } )

  expect_doppelganger("Polar plot with growing, fullheight", {
    ggplot(gold, aes(label = line, xmin = xmin, xmax = xmax, ymin = ymin, 
                     ymax = ymax)) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(grow = TRUE, fullheight = TRUE, min.size = 0)
  } )

  expect_doppelganger("Polar plot with text placed at the top", {
    ggplot(gold, aes(label = line, xmin = xmin, xmax = xmax, ymin = ymin, 
                     ymax = ymax)) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(place = "top", min.size = 0)
  } )

  expect_doppelganger("Polar plot with text placed at the bottom", {
    ggplot(gold, aes(label = line, xmin = xmin, xmax = xmax, ymin = ymin, 
                     ymax = ymax)) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(place = "bottom", min.size = 0)
  } )


} )


test_that("geom_fit_text() emits an error if you try to draw rich text in polar coordinates", {

  expect_error( {
    p <- ggplot(gold, aes(label = line, xmin = xmin, xmax = xmax, ymin = ymin, 
                     ymax = ymax)) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(min.size = 0, rich = TRUE)
    print(p)
  } )

} )


test_that("geom_fit_text() correctly applies width and height arguments in polar coordinates", {

  expect_doppelganger("Polar plot with height of 20 mm", {
    ggplot(gold) +
      coord_polar() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      geom_fit_text(
        aes(label = line, xmin = xmin, xmax = xmax, y = (ymin + ymax) / 2),
        min.size = 0, grow = TRUE, height = grid::unit(20, "mm")
      )
  } )

  expect_doppelganger("Polar plot with height of 5 mm", {
    ggplot(gold) +
      coord_polar() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      geom_fit_text(
        aes(label = line, xmin = xmin, xmax = xmax, y = (ymin + ymax) / 2),
        min.size = 0, grow = TRUE, height = grid::unit(5, "mm")
      )
  } )

  expect_doppelganger("Polar plot with width of 100 mm", {
    ggplot(gold) +
      coord_polar() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      geom_fit_text(
        aes(label = line, x = (xmin + xmax) / 2, ymin = ymin, ymax = ymax),
        min.size = 0, grow = TRUE, width = grid::unit(100, "mm")
      )
  } )

  expect_doppelganger("Polar plot with width of 40 mm", {
    ggplot(gold) +
      coord_polar() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      geom_fit_text(
        aes(label = line, x = (xmin + xmax) / 2, ymin = ymin, ymax = ymax),
        min.size = 0, grow = TRUE, width = grid::unit(40, "mm")
      )
  } )

} )

test_that("The flip argument works properly when drawing in polar coordinates", {

  expect_doppelganger("Polar plot with flip", {
    ggplot(gold, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = line)) +
      coord_polar() +
      geom_rect() +
      geom_fit_text(min.size = 0, flip = TRUE, grow = TRUE)
  })

  expect_doppelganger("Polar plot with flip and shifted start", {
    ggplot(gold, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = line)) +
      coord_polar(start = (pi / 2)) +
      geom_rect() +
      geom_fit_text(min.size = 0, flip = TRUE, grow = TRUE)
  })

  expect_doppelganger("Polar plot with flip and text in top left", {
    ggplot(gold, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = line)) +
      coord_polar() +
      geom_rect() +
      geom_fit_text(min.size = 0, flip = TRUE, place = "topleft")
  })

} )
