library(ggplot2)
library(vdiffr)

test_that("geom_bar_text() draws test with positions, growing and reflowing", {

  expect_doppelganger("Simple geom_bar_text() plot", {
    ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
      geom_col() +
      geom_bar_text()
  } )

  expect_doppelganger("Stacked labels", {
    ggplot(beverages, aes(x = beverage, y = proportion, label = ingredient,
                          fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack")
  } )

  expect_doppelganger("Stacked labels, growing and reflowing", {
    ggplot(beverages, aes(x = beverage, y = proportion, label = ingredient,
                          fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE)
  } )

  expect_doppelganger("Dodged labels, growing and reflowing", {
    ggplot(beverages, aes(x = beverage, y = proportion, label = ingredient,
                          fill = ingredient)) +
      geom_col(position = "dodge") +
      geom_bar_text(position = "dodge", grow = TRUE, reflow = TRUE, place = "left")
  } )

  expect_doppelganger("Stacked bar plot with '0' values", {
    beverages2 <- beverages
    beverages2[1, 3] <- 0
    ggplot(beverages2, aes(x = beverage, y = proportion, label = ingredient,
                        fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE, size = 48)
  } )

} )

test_that("geom_bar_text() correctly handles explicit and implied flipped axes", {

  expect_doppelganger("Bar plot with stacked labels and coord_flip()", {
    ggplot(beverages, aes(x = beverage, y = proportion, label = ingredient,
                          fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE, place = "left") +
      coord_flip()
  } )

  expect_doppelganger("Bar plot with dodged labels and coord_flip()", {
    ggplot(beverages, aes(x = beverage, y = proportion, label = ingredient,
                          fill = ingredient)) +
      geom_col(position = "dodge") +
      geom_bar_text(position = "dodge", grow = TRUE, reflow = TRUE, place = "left") +
      coord_flip()
  } )

  expect_doppelganger("Bar plot with implied axis flip", {
    ggplot(altitudes, aes(x = altitude, y = craft, label = altitude)) +
      geom_col() +
      geom_bar_text(grow = TRUE)
  } )

  expect_doppelganger("Bar plot with coord_flip() and place = middle", {
    ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
      coord_flip() +
      geom_col() +
      geom_bar_text(place = "middle")
  } )

  expect_doppelganger("Bar plot with implied axis flip and place = middle", {
    ggplot(altitudes, aes(x = altitude, y = craft, label = altitude)) +
      geom_col() +
      geom_bar_text(place = "middle")
  } )

  expect_doppelganger("Integer y shouldn't induce an implied flip", {
    beverages2 <- beverages
    beverages2$proportion <- 1:6
    ggplot(beverages2, aes(x = beverage, y = proportion, label = ingredient,
                        fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack")
  } )

} )

test_that("geom_bar_text() works with faceting", {

  expect_doppelganger("geom_bar_text() with faceting", {
    ggplot(beverages, aes(x = beverage, y = proportion, label = ingredient,
                          fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE, place = "left") +
      facet_wrap(~ beverage)
  } )

} )

test_that("geom_bar_text() draws contrasting text", {

  expect_doppelganger("Contrast against default bar colour", {
    ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
      geom_col() +
      geom_bar_text(contrast = TRUE)
  } )

  expect_doppelganger("Contrasting with non-black text", {
    ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +
      geom_tile() +
      geom_fit_text(colour = "thistle", reflow = TRUE, grow = TRUE, contrast = TRUE) +
       scale_fill_gradientn(
         colours = c("red", "yellow", "green", "lightblue", "darkblue"),
         values = c(1.0, 0.8, 0.6, 0.4, 0.2, 0)
       ) 
  } )

} )
