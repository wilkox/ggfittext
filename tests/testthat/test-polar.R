library(ggplot2)
library(vdiffr)
grid::current.viewport()

polar_d <- data.frame(
  word = c("The" , "falcon" , "cannot" , "hear" , "the" , "falconer") ,
  xmin = c( 0.1  , 0.3      , 0.6      , 0.8    , 0.5   , 0.2)        ,
  xmax = c( 0.2  , 0.55     , 0.8      , 1.0    , 0.7   , 0.6)        ,
  ymin = c( 0.65 , 0.45     , 0.55     , 0.4    , 0.25  , 0.1)        ,
  ymax = c( 0.95 , 0.8      , 0.95     , 0.5    , 0.35  , 0.2)
)

context("plots in polar coordinates")

test_that("basic plots in polar coordinates run without errors", {

  expect_silent( {
    p <- ggplot(polar_d, aes(label = word, xmin = xmin, xmax = xmax, ymin =
                             ymin, ymax = ymax)) +
      geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1)) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(
        grow = TRUE,
        padding.x = grid::unit(2, "mm"),
        padding.y = grid::unit(2, "mm"),
        place = "bottom",
        min.size = 0
      )
    print(p)
  } )

  expect_silent( {
    p <- ggplot(gold, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = label)) +
      geom_rect() +
      geom_fit_text(aes(x = 1, y = 1), grow = TRUE, fullheight = TRUE) +
      coord_polar()
    print(p)
  } )
} )

context("visual tests of plots")

test_that("plots look the way they should", {

  expect_doppelganger("place = top", {
    ggplot(polar_d, aes(label = word, xmin = xmin, xmax = xmax, ymin =
                             ymin, ymax = ymax)) +
      geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1)) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(
        grow = TRUE,
        padding.x = grid::unit(2, "mm"),
        padding.y = grid::unit(2, "mm"),
        place = "top",
        min.size = 0
      )
  } )

  expect_doppelganger("place = centre", {
    ggplot(polar_d, aes(label = word, xmin = xmin, xmax = xmax, ymin =
                             ymin, ymax = ymax)) +
      geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1)) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(
        grow = TRUE,
        padding.x = grid::unit(2, "mm"),
        padding.y = grid::unit(2, "mm"),
        place = "centre",
        min.size = 0
      )
  } )

  expect_doppelganger("place = bottom", {
    ggplot(polar_d, aes(label = word, xmin = xmin, xmax = xmax, ymin =
                             ymin, ymax = ymax)) +
      geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1)) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(
        grow = TRUE,
        padding.x = grid::unit(2, "mm"),
        padding.y = grid::unit(2, "mm"),
        place = "bottom",
        min.size = 0
      )
  } )

  expect_doppelganger("frost poem", {
    ggplot(gold, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                     label = label, fill = 1:8)) +
      geom_rect() +
      coord_polar() +
      geom_fit_text(grow = TRUE, fullheight = TRUE, place = "middle", min.size = 0) +
      scale_fill_gradient(low = "#fee391", high = "#238443")
  } )

  expect_doppelganger("height of 20 mm", {
    ggplot(gold) +
      coord_polar() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                    ymax = ymax, fill = line)) +
      geom_fit_text(
        aes(label = label, xmin = xmin, xmax = xmax, y = (ymin + ymax) / 2),
        min.size = 0, grow = TRUE, height = grid::unit(20, "mm")
      ) +
      scale_fill_gradient(low = "#fee391", high = "#238443")
  } )

  expect_doppelganger("height of 5 mm", {
    ggplot(gold) +
      coord_polar() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                    ymax = ymax, fill = line)) +
      geom_fit_text(
        aes(label = label, xmin = xmin, xmax = xmax, y = (ymin + ymax) / 2),
        min.size = 0, grow = TRUE, height = grid::unit(5, "mm")
      ) +
      scale_fill_gradient(low = "#fee391", high = "#238443")
  } )

  expect_doppelganger("width of 100 mm", {
    ggplot(gold) +
      coord_polar() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                    ymax = ymax, fill = line)) +
      geom_fit_text(
        aes(label = label, ymin = ymin, ymax = ymax, x = (xmin + xmax) / 2),
        min.size = 0, grow = TRUE, width = grid::unit(100, "mm")
      ) +
      scale_fill_gradient(low = "#fee391", high = "#238443")
  } )

  expect_doppelganger("width of 40 mm", {
    ggplot(gold) +
      coord_polar() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, 
                    ymax = ymax, fill = line)) +
      geom_fit_text(
        aes(label = label, ymin = ymin, ymax = ymax, x = (xmin + xmax) / 2),
        min.size = 0, grow = TRUE, width = grid::unit(40, "mm")
      ) +
      scale_fill_gradient(low = "#fee391", high = "#238443")
  } )

} )
