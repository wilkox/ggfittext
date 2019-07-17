library(ggplot2)
library(ggfittext)

yeats <- data.frame(
  xmin = c(0, 4, 6, 4, 4, 5, 5.5, 5,   5,    5.25, 5.25),
  xmax = c(4, 8, 8, 6, 5, 6, 6,   5.5, 5.25, 5.5,  5.5),
  ymin = c(0, 4, 0, 0, 2, 3, 2,   2,   2.5,  2.75, 2.5),
  ymax = c(8, 8, 4, 2, 4, 4, 3,   2.5, 3,    3,    2.75),
  label = c("Turning", "and", "turning", "in", "the", "widening", "gyre", "the",
            "falcon", "cannot", "hear"),
  angle = c(0, 315, 270, 225, 180, 135, 90, 45, 0, 315, 270)
)

context("visual tests of plots")

test_that("plots look the way they should", {

  vdiffr::expect_doppelganger("Angles and basic placement", {
    ggplot(yeats, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label =
                  label, angle = angle)) +
    geom_rect(fill = "grey", colour = "black") +
    geom_fit_text(grow = TRUE, min.size = 0, size = 48)
  })

  vdiffr::expect_doppelganger("Basic placement", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(size = 48)
  })

  vdiffr::expect_doppelganger("Basic reflowing", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(reflow = TRUE, size = 48)
  })

  vdiffr::expect_doppelganger("Reflowing and growing", {
    ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(reflow = TRUE, grow = TRUE, size = 48)
  })

  vdiffr::expect_doppelganger("Placing", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(place = "topleft", reflow = TRUE, size = 48)
  })

  vdiffr::expect_doppelganger("Basic bar plot", {
    ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
      geom_col() +
      geom_bar_text(size = 48)
  })

  vdiffr::expect_doppelganger("Stacked bar plot", {
    ggplot(coffees, aes(x = coffee, y = proportion, label = ingredient,
                        fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE, size = 48)
  })

  vdiffr::expect_doppelganger("Dodged and flipped bar plot", {
    ggplot(coffees, aes(x = coffee, y = proportion, label = ingredient,
                        fill = ingredient)) +
      geom_col(position = "dodge") +
      geom_bar_text(position = "dodge", grow = TRUE, reflow = TRUE, 
                    place = "left", size = 48) +
      coord_flip()
  })

  vdiffr::expect_doppelganger("ymin/ymax", {
    ggplot(presidential, aes(ymin = start, ymax = end, x = party, label = name)) +
      geom_fit_text(grow = TRUE, size = 48) +
      geom_errorbar(alpha = 0.5)
  })

  vdiffr::expect_doppelganger("Contrasting text", {
    ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +
      geom_tile() +
      geom_fit_text(reflow = TRUE, grow = TRUE, contrast = TRUE, size = 48)
  })

  vdiffr::expect_doppelganger("Blank labels", {
    presidential$name[1] <- ""
    ggplot(presidential, aes(ymin = start, ymax = end, label = name, x = party)) +
        geom_fit_text(grow = TRUE)
  })
})
