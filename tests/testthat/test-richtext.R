grid::current.viewport()

context("rich text")

test_that("simple plots with rich text work", {

  expect_silent({
    p <- ggplot(animals_rich, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(rich = TRUE)
    print(p)
  })

  expect_doppelganger("simple rich text plot", {
   ggplot(animals_rich, aes(x = type, y = flies, label = animal)) +
     geom_tile(fill = "white", colour = "black") +
     geom_fit_text(rich = TRUE)
  })

  expect_silent({
    p <- ggplot(animals_rich, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(rich = TRUE, grow = TRUE)
    print(p)
  })

  expect_doppelganger("simple rich text plot with grow", {
   ggplot(animals_rich, aes(x = type, y = flies, label = animal)) +
     geom_tile(fill = "white", colour = "black") +
     geom_fit_text(rich = TRUE, grow = TRUE)
  })

  expect_silent({
    p <- ggplot(animals_rich, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(rich = TRUE, grow = TRUE, reflow = TRUE)
    print(p)
  })

  expect_doppelganger("simple rich text plot with grow and reflow", {
   ggplot(animals_rich, aes(x = type, y = flies, label = animal)) +
     geom_tile(fill = "white", colour = "black") +
     geom_fit_text(rich = TRUE, grow = TRUE, reflow = TRUE)
  })
})
