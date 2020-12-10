library(ggplot2)
grid::current.viewport()

yeats <- data.frame(
  xmin = c(0, 4, 6, 4, 4, 5, 5.5, 5,   5,    5.25, 5.25),
  xmax = c(4, 8, 8, 6, 5, 6, 6,   5.5, 5.25, 5.5,  5.5),
  ymin = c(0, 4, 0, 0, 2, 3, 2,   2,   2.5,  2.75, 2.5),
  ymax = c(8, 8, 4, 2, 4, 4, 3,   2.5, 3,    3,    2.75),
  line = c("Turning", "and", "turning", "in", "the", "widening", "gyre", "the",
            "falcon", "cannot", "hear"),
  angle = c(0, 315, 270, 225, 180, 135, 90, 45, 0, 315, 270)
)

context("geom_bar_text()")

test_that("plots look the way they should", {

  expect_doppelganger("coord_flip", {
    ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
      coord_flip() +
      geom_col() +
      geom_bar_text()
  })

  expect_doppelganger("implied flip", {
    ggplot(altitudes, aes(y = craft, x = altitude, label = altitude)) +
      geom_col() +
      geom_bar_text()
  })

  expect_doppelganger("coord_flip with grow", {
    ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
      coord_flip() +
      geom_col() +
      geom_bar_text(grow = TRUE)
  })

  expect_doppelganger("implied flip with grow", {
    ggplot(altitudes, aes(y = craft, x = altitude, label = altitude)) +
      geom_col() +
      geom_bar_text(grow = TRUE)
  })

  expect_doppelganger("coord_flip with place middle", {
    ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
      coord_flip() +
      geom_col() +
      geom_bar_text(place = "middle")
  })

  expect_doppelganger("implied flip with place middle", {
    ggplot(altitudes, aes(y = craft, x = altitude, label = altitude)) +
      geom_col() +
      geom_bar_text(place = "middle")
  })

  expect_doppelganger("coord_flip with dodge", {
    ggplot(beverages, aes(x = beverage, y = proportion, label = ingredient,
                        fill = ingredient)) +
      geom_col(position = "dodge") +
      geom_bar_text(position = "dodge") +
      coord_flip()
  })

  expect_doppelganger("coord_flip with dodge and grow and reflow", {
    ggplot(beverages, aes(x = beverage, y = proportion, label = ingredient,
                        fill = ingredient)) +
      geom_col(position = "dodge") +
      geom_bar_text(position = "dodge", grow = TRUE, reflow = TRUE, 
                    place = "left") +
      coord_flip()
  })

  expect_doppelganger("implied flip with dodge", {
    ggplot(beverages, aes(y = beverage, x = proportion, label = ingredient,
                        fill = ingredient)) +
      geom_col(position = "dodge") +
      geom_bar_text(position = "dodge")
  })

  expect_doppelganger("implied flip with dodge and grow and reflow", {
    ggplot(beverages, aes(y = beverage, x = proportion, label = ingredient,
                        fill = ingredient)) +
      geom_col(position = "dodge") +
      geom_bar_text(position = "dodge", grow = TRUE, reflow = TRUE, 
                    place = "left")
  })


  expect_doppelganger("implied doesn't misfire with integer y", {
    c2 <- beverages
    c2$proportion <- 1:6
    ggplot(c2, aes(x = beverage, y = proportion, label = ingredient,
                        fill = ingredient)) +
    geom_col(position = "dodge") +
    geom_bar_text(position = "dodge")
  })

})

context("visual tests of plots")

test_that("plots look the way they should", {

  expect_doppelganger("Angles and basic placement", {
    ggplot(yeats, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label =
                  line, angle = angle)) +
    geom_rect(fill = "grey", colour = "black") +
    geom_fit_text(grow = TRUE, min.size = 0, size = 48)
  })

  expect_doppelganger("Basic placement", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(size = 48)
  })

  expect_doppelganger("Basic reflowing", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(reflow = TRUE, size = 48)
  })

  expect_doppelganger("Reflowing and growing", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(reflow = TRUE, grow = TRUE, size = 48)
  })

  expect_doppelganger("Placing", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(place = "topleft", reflow = TRUE, size = 48)
  })

  expect_doppelganger("Basic bar plot", {
    ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
      geom_col() +
      geom_bar_text(size = 48)
  })

  expect_doppelganger("Stacked bar plot", {
    ggplot(beverages, aes(x = beverage, y = proportion, label = ingredient,
                        fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE, size = 48)
  })

  expect_doppelganger("Stacked bar plot with '0' value", {
    beverages2 <- beverages
    beverages2[1, 3] <- 0
    ggplot(beverages2, aes(x = beverage, y = proportion, label = ingredient,
                        fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE, size = 48)
  })

  expect_doppelganger("Stacked and flipped bar plot", {
    ggplot(beverages, aes(x = beverage, y = proportion, label = ingredient,
                        fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE, size = 48) +
      coord_flip()
  })

  expect_doppelganger("Dodged and flipped bar plot", {
    ggplot(beverages, aes(x = beverage, y = proportion, label = ingredient,
                        fill = ingredient)) +
      geom_col(position = "dodge") +
      geom_bar_text(position = "dodge", grow = TRUE, reflow = TRUE, 
                    place = "left", size = 48) +
      coord_flip()
  })

  expect_doppelganger("ymin/ymax", {
    ggplot(presidential, aes(ymin = start, ymax = end, x = party, label = name)) +
      geom_fit_text(grow = TRUE, size = 48) +
      geom_errorbar(alpha = 0.5)
  })

  expect_doppelganger("Contrasting text", {
    ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +
      geom_tile() +
      geom_fit_text(reflow = TRUE, grow = TRUE, contrast = TRUE, size = 48)
  })

  expect_doppelganger("Blank labels", {
    pressies <- presidential
    pressies$name[1] <- ""
    ggplot(pressies, aes(ymin = start, ymax = end, label = name, x = party)) +
        geom_fit_text(grow = TRUE)
  })

  expect_doppelganger("Contrast against default bar colour", {
    ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
      geom_col() +
      geom_bar_text(contrast = TRUE)
  })

  expect_doppelganger("Contrasting works with non-black text", {
    ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +
      geom_tile() +
      geom_fit_text(colour = "thistle", reflow = TRUE, grow = TRUE, 
                    contrast = TRUE) +
       scale_fill_gradientn(
         colours = c("red","yellow","green","lightblue","darkblue"),
         values = c(1.0,0.8,0.6,0.4,0.2,0)
       ) 
  })

})
