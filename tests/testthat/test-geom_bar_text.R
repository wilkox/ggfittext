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
    p <- ggplot2::ggplot(beverages, ggplot2::aes(x = beverage, y = proportion, 
                                               label = ingredient, fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE)
    print(p)
  } )

  expect_silent( {
    p <- ggplot2::ggplot(beverages, ggplot2::aes(x = beverage, y = proportion, 
                                               label = ingredient, fill = ingredient)) +
      geom_col(position = "dodge") +
      geom_bar_text(position = "dodge", grow = TRUE, reflow = TRUE, 
                    place = "left") +
      coord_flip()
    print(p)
  } )

  expect_silent( {
    df <- data.frame(
      stringsAsFactors = FALSE,
      x = c(10,10,25,25,50,50,100,100,200,
            200,500,500),
      variable = c("AAA","nAAA","AAA","nAAA","AAA",
                   "nAAA","AAA","nAAA","AAA","nAAA","AAA",
                   "nAAA"),
      value = c(-4.5,-0.1,-1.2,0,-34.1,-1.8,-38.2,
                -2.1,-3.8,-0.1,0,0)
    )
    p <- ggplot(df, aes(x = factor(x), y = value, label = value)) +
      facet_wrap(variable ~ .) +
      geom_col() +
      geom_bar_text()
    print(p)
  } )
} )
