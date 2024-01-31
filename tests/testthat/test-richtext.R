library(ggplot2)

test_that("geom_fit_text() plots with rich text", {

  skip_on_os(c("windows", "linux"))

  expect_doppelganger("Basic rich text", {
    ggplot(animals_rich, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(rich = TRUE)
  } )

  expect_doppelganger("Basic rich text with grow", {
   ggplot(animals_rich, aes(x = type, y = flies, label = animal)) +
     geom_tile(fill = "white", colour = "black") +
     geom_fit_text(rich = TRUE, grow = TRUE)
  } )

  expect_doppelganger("Basic rich text with reflow", {
   ggplot(animals_rich, aes(x = type, y = flies, label = animal)) +
     geom_tile(fill = "white", colour = "black") +
     geom_fit_text(rich = TRUE, reflow = TRUE)
  } )

  expect_doppelganger("Basic rich text with grow and reflow", {
   ggplot(animals_rich, aes(x = type, y = flies, label = animal)) +
     geom_tile(fill = "white", colour = "black") +
     geom_fit_text(rich = TRUE, grow = TRUE, reflow = TRUE)
  } )

  animals_rich2 <- animals_rich
  animals_rich2$animal[1] <- paste(
    "Whose **woods these *are* I** < think <sub>I</sub> <span>know",
    "</span>. His <sup>house</sup> **is** in > *the* ***village*** though.",
    "What do ****four asterisks**** do?<br>",
    "What about *****five asterisks*****?",
    "This is < less <sub>than</sub> <sup>this</sup>, and a * couple of ** floating asterisks.",
    sep = " "
  )
  animals_rich2$animal[6] <- paste(
    "<span style='color:red'",
    ">",
    "red</span>-rumped parrot",
    sep = " "
  )

  expect_doppelganger("Complex rich text with grow and reflow", {
    ggplot(animals_rich2, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(rich = TRUE, grow = TRUE, reflow = TRUE)
  } )

  expect_doppelganger("Complex rich text with reflow only", {
    ggplot(animals_rich2, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(rich = TRUE, reflow = TRUE)
  } )

} )

test_that("geom_bar_text() plots with rich text", {

  skip_on_os(c("windows", "linux"))

  expect_doppelganger("Basic bar plot with rich text", {
    ggplot(beverages_rich, aes(x = beverage, y = proportion, label = ingredient, 
                               fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", rich = TRUE)
  } )

  expect_doppelganger("Rich bar plot with grow", {
    ggplot(beverages_rich, aes(x = beverage, y = proportion, label = ingredient, 
                               fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", rich = TRUE, grow = TRUE)
  } )

  expect_doppelganger("Rich bar plot with reflow", {
    ggplot(beverages_rich, aes(x = beverage, y = proportion, label = ingredient, 
                               fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", rich = TRUE, reflow = TRUE)
  } )

  expect_doppelganger("Rich bar plot with grow and reflow", {
    ggplot(beverages_rich, aes(x = beverage, y = proportion, label = ingredient, 
                               fill = ingredient)) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", rich = TRUE, grow = TRUE, reflow = TRUE)
  } )

} )
