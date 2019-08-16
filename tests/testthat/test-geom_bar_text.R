context("basic bar plots")
library(ggplot2)
grid::current.viewport()

test_that("simple bar plots and options do not produce errors", {
  expect_silent( {
    p <- ggplot2::ggplot(altitudes, ggplot2::aes(x = craft, y = altitude, 
                                                 label = altitude)) +
      geom_col() +
      geom_bar_text()
    print(p)
  } )

  expect_silent( {
    p <- ggplot2::ggplot(coffees, ggplot2::aes(x = coffee, y = proportion, 
                                               label = ingredient, fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE)
    print(p)
  } )

  expect_silent( {
    p <- ggplot2::ggplot(coffees, ggplot2::aes(x = coffee, y = proportion, 
                                               label = ingredient, fill = ingredient)) +
      geom_col(position = "dodge") +
      geom_bar_text(position = "dodge", grow = TRUE, reflow = TRUE, 
                    place = "left") +
      coord_flip()
    print(p)
  } )
} )
