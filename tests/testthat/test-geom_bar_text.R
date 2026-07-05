library(ggplot2)
library(vdiffr)

test_that("geom_bar_text() draws test with positions, growing and reflowing", {
  expect_doppelganger("Simple geom_bar_text() plot", {
    ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
      geom_col() +
      geom_bar_text()
  })

  expect_doppelganger("Stacked labels", {
    ggplot(
      beverages,
      aes(x = beverage, y = proportion, label = ingredient, fill = ingredient)
    ) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack")
  })

  expect_doppelganger("Stacked labels, growing and reflowing", {
    ggplot(
      beverages,
      aes(x = beverage, y = proportion, label = ingredient, fill = ingredient)
    ) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE)
  })

  expect_doppelganger("Dodged labels, growing and reflowing", {
    ggplot(
      beverages,
      aes(x = beverage, y = proportion, label = ingredient, fill = ingredient)
    ) +
      geom_col(position = "dodge") +
      geom_bar_text(
        position = "dodge",
        grow = TRUE,
        reflow = TRUE,
        place = "left"
      )
  })

  expect_doppelganger("Stacked bar plot with '0' values", {
    beverages2 <- beverages
    beverages2[1, 3] <- 0
    ggplot(
      beverages2,
      aes(x = beverage, y = proportion, label = ingredient, fill = ingredient)
    ) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE, size = 48)
  })
})

test_that("geom_bar_text() correctly handles explicit and implied flipped axes", {
  expect_doppelganger("Bar plot with stacked labels and coord_flip()", {
    ggplot(
      beverages,
      aes(x = beverage, y = proportion, label = ingredient, fill = ingredient)
    ) +
      geom_col(position = "stack") +
      geom_bar_text(
        position = "stack",
        grow = TRUE,
        reflow = TRUE,
        place = "left"
      ) +
      coord_flip()
  })

  expect_doppelganger("Bar plot with dodged labels and coord_flip()", {
    ggplot(
      beverages,
      aes(x = beverage, y = proportion, label = ingredient, fill = ingredient)
    ) +
      geom_col(position = "dodge") +
      geom_bar_text(
        position = "dodge",
        grow = TRUE,
        reflow = TRUE,
        place = "left"
      ) +
      coord_flip()
  })

  expect_doppelganger("Bar plot with implied axis flip", {
    ggplot(altitudes, aes(x = altitude, y = craft, label = altitude)) +
      geom_col() +
      geom_bar_text(grow = TRUE)
  })

  expect_doppelganger("Bar plot with coord_flip() and place = middle", {
    ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
      coord_flip() +
      geom_col() +
      geom_bar_text(place = "middle")
  })

  expect_doppelganger("Bar plot with implied axis flip and place = middle", {
    ggplot(altitudes, aes(x = altitude, y = craft, label = altitude)) +
      geom_col() +
      geom_bar_text(place = "middle")
  })

  expect_doppelganger("Integer y shouldn't induce an implied flip", {
    beverages2 <- beverages
    beverages2$proportion <- 1:6
    ggplot(
      beverages2,
      aes(x = beverage, y = proportion, label = ingredient, fill = ingredient)
    ) +
      geom_col(position = "stack") +
      geom_bar_text(position = "stack")
  })
})

test_that("horizontal bars are labelled with the value, not the category index (#45)", {
  p <- ggplot(
    data.frame(cat = c("a", "b"), val = c(10, 20)),
    aes(x = val, y = cat)
  ) +
    geom_col() +
    geom_bar_text()
  expect_equal(layer_data(p, 2)$label, c(10, 20))
})

test_that("geom_bar_text() works with faceting", {
  expect_doppelganger("geom_bar_text() with faceting", {
    ggplot(
      beverages,
      aes(x = beverage, y = proportion, label = ingredient, fill = ingredient)
    ) +
      geom_col(position = "stack") +
      geom_bar_text(
        position = "stack",
        grow = TRUE,
        reflow = TRUE,
        place = "left"
      ) +
      facet_wrap(~beverage)
  })
})

test_that("geom_bar_text() draws contrasting text", {
  expect_doppelganger("Contrast against default bar colour", {
    ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
      geom_col() +
      geom_bar_text(contrast = TRUE)
  })

  expect_doppelganger("Contrasting with non-black text", {
    ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +
      geom_tile() +
      geom_fit_text(
        colour = "thistle",
        reflow = TRUE,
        grow = TRUE,
        contrast = TRUE
      ) +
      scale_fill_gradientn(
        colours = c("red", "yellow", "green", "lightblue", "darkblue"),
        values = c(1.0, 0.8, 0.6, 0.4, 0.2, 0)
      )
  })
})

test_that("contrast auto-detection tolerates NA colours (#56)", {
  # When contrast is left to auto-detect and every non-NA text colour is the
  # default black, a single NA colour previously made all(colour == "black")
  # evaluate to NA, crashing makeContent() with "missing value where
  # TRUE/FALSE needed". Auto-detection should ignore NA colours instead.
  df <- data.frame(
    cat = c("a", "b", "c"),
    val = c(1, 1, 1),
    txt = c("black", "black", NA)
  )
  p <- ggplot(df, aes(x = cat, y = val, label = cat, colour = txt)) +
    geom_col() +
    geom_bar_text() +
    scale_colour_identity()

  pdf(NULL)
  on.exit(dev.off())
  expect_no_error(print(p))
})

test_that("outside default is derived from position (string or object)", {
  # position_identity(): outside should default to TRUE, whether the position
  # is given as a string or as a Position object (#44)
  expect_true(geom_bar_text(position = "identity")$geom_params$outside)
  expect_true(geom_bar_text(position = position_identity())$geom_params$outside)

  # Any other position defaults outside to FALSE. A Position object is required
  # whenever a dodge width must be set, and comparing it with == would error.
  expect_false(geom_bar_text(position = "stack")$geom_params$outside)
  expect_false(
    geom_bar_text(position = position_dodge(width = 0.9))$geom_params$outside
  )

  # An explicit outside is always honoured over the position-derived default.
  expect_false(
    geom_bar_text(position = "identity", outside = FALSE)$geom_params$outside
  )
})

# geom_bar_text() nests one fittexttree per sign group inside an outer gTree, so
# we reach into the positive group and draw it directly. The short bar (0.1)
# cannot fit its label inside; the tall bar (100) always can. So the short-bar
# label is dropped when outside = FALSE and drawn above the bar when
# outside = TRUE.
test_that("geom_bar_text() draws short-bar labels outside the bar (#44)", {
  df <- data.frame(craft = c("a", "b"), altitude = c(0.1, 100))
  draw <- function(outside) {
    p <- ggplot(df, aes(x = craft, y = altitude, label = altitude)) +
      geom_col() +
      geom_bar_text(outside = outside, min.size = 8)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    outer <- suppressWarnings(layer_grob(p, 2)[[1]])
    # Both bars are positive, so there is a single fittexttree (the positives
    # group). Assert that structure before reaching into it, so a refactor of
    # the nesting fails here rather than silently drawing the wrong group.
    expect_length(outer$children, 1)
    expect_s3_class(outer$children[[1]], "fittexttree")
    grid::makeContent(outer$children[[1]])$children
  }

  # The tall bar is always labelled; the short bar is only labelled when its
  # label may be drawn outside the bar.
  expect_length(draw(FALSE), 1)
  expect_length(draw(TRUE), 2)
})

# A valid formatter is applied to the labels, including the value-axis labels
# geom_bar_text() infers when no label aesthetic is mapped.
test_that("geom_bar_text() applies a valid formatter to the labels", {
  p <- ggplot(data.frame(x = c("a", "b"), y = c(1, 2)), aes(x, y)) +
    geom_col() +
    geom_bar_text(formatter = function(x) paste0(x, "%"))
  expect_equal(layer_data(p, 2)$label, c("1%", "2%"))
})

# GeomBarText carries its own copy of the formatter validation, parallel to
# GeomFitText's; exercise its error paths independently so the two cannot drift
# apart unnoticed.
test_that("geom_bar_text() rejects invalid formatters", {
  base <- ggplot(data.frame(x = c("a", "b"), y = c(1, 2)), aes(x, y)) +
    geom_col()

  # Not a function at all (the geom's own check).
  expect_error(
    ggplot_build(base + geom_bar_text(formatter = "nope")),
    "must be a function"
  )
  # Wrong type and wrong length are rejected by vapply's template enforcement.
  expect_error(ggplot_build(base + geom_bar_text(formatter = function(x) 1)))
  expect_error(
    ggplot_build(base + geom_bar_text(formatter = function(x) c(x, x)))
  )
})

test_that("width works as expected", {
  expect_doppelganger("geom_bar_text() with width to match geom_col() width", {
    ggplot(
      beverages,
      aes(x = beverage, y = proportion, label = ingredient, fill = ingredient)
    ) +
      geom_col(position = "stack", width = 0.5) +
      geom_bar_text(
        position = "stack",
        reflow = TRUE,
        width = 0.5,
        grow = TRUE,
        fullheight = TRUE
      )
  })
})
