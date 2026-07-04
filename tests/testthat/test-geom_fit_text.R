library(ggplot2)
library(vdiffr)

test_that("geom_fit_text() places text correctly", {
  expect_doppelganger("Default placement (middle)", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text()
  })

  expect_doppelganger("Placing at the top", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(place = "top")
  })

  expect_doppelganger("Placing at the bottom right", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(place = "bottomright")
  })
})

test_that("geom_fit_text() grows and reflows text correctly", {
  expect_doppelganger("Growing without reflowing", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(grow = TRUE)
  })

  expect_doppelganger("Growing with placement at the bottom", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(grow = TRUE, place = "bottom")
  })

  expect_doppelganger("Growing with reflowing", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(grow = TRUE, reflow = TRUE)
  })

  expect_doppelganger("Reflowing without growing", {
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(reflow = TRUE)
  })

  animals2 <- animals
  animals2$animal[1] <- paste(
    "Whose woods these are I think I know.",
    "His house is in the village though;",
    "He will not see me stopping here",
    "To watch his woods fill up with snow.",
    sep = "\n"
  )
  animals2$animal[2] <- paste(
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
  })

  expect_doppelganger("Complex text with reflowing only", {
    ggplot(animals2, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(reflow = TRUE)
  })
})

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
  })
})

test_that("The formatter argument works correctly", {
  expect_doppelganger("Label with formatter function", {
    wikif <- function(x) {
      paste0(x, " (citation needed)")
    }
    ggplot(animals, aes(x = type, y = flies, label = animal)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(grow = TRUE, reflow = TRUE, formatter = wikif)
  })
})

test_that("geom_fit_text() handles blank labels", {
  expect_doppelganger("Blank label", {
    presidential$name[1] <- ""
    ggplot(
      presidential,
      aes(ymin = start, ymax = end, label = name, x = party)
    ) +
      geom_fit_text(grow = TRUE)
  })
})

# Logic-level test for the blank-label filter (#48). The vdiffr snapshot above
# would still pass with the old, accidentally-working condition, so we inspect
# the drawn grob tree directly: NA and empty labels must be dropped, leaving one
# child grob for the single real label.
test_that("makeContent.fittexttree() drops NA and empty labels (#48)", {
  df <- data.frame(x = 1:3, y = 1:3, label = c("a", NA, ""))
  p <- ggplot(df, aes(x, y, label = label)) + geom_fit_text()

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())

  ftt <- suppressWarnings(layer_grob(p)[[1]])
  drawn <- grid::makeContent(ftt)
  expect_length(drawn$children, 1)
})

test_that("geom_fit_text() warns when box limits are outside of plot limits", {
  df <- data.frame(x = letters[1:5], y = 0:4, lb = 3)

  expect_warning(
    {
      p <- ggplot(df, aes(x = x, y = y, label = lb)) +
        geom_fit_text() +
        ylim(0.5, 3)
      print(p)
    },
    "box limits were outside plot limits"
  )

  expect_warning(
    {
      p <- ggplot(df, aes(x = x, y = y, label = lb)) +
        ylim(-0.1, 6) +
        geom_fit_text()
      print(p)
    },
    "box limits were outside plot limits"
  )

  expect_warning(
    {
      p <- ggplot(df, aes(x = y, y = x, label = lb)) +
        xlim(-0.1, 6) +
        geom_fit_text()
      print(p)
    },
    "box limits were outside plot limits"
  )
})

test_that("geom_fit_text() draws angled text correctly", {
  expect_doppelganger("Angled text", {
    ggplot(animals, aes(x = type, y = flies, label = animal, angle = mass)) +
      geom_tile(fill = "white", colour = "black") +
      geom_fit_text(size = 48)
  })
})

test_that("geom_fit_text() draws in boxes defined with xmin/xmax and ymin/ymax", {
  expect_doppelganger("Box defined with x and ymin/ymax", {
    ggplot(
      presidential,
      aes(ymin = start, ymax = end, x = party, label = name)
    ) +
      geom_fit_text(grow = TRUE, size = 48) +
      geom_errorbar(alpha = 0.5)
  })

  expect_doppelganger("Box defined with xmin/xmax and y", {
    ggplot(
      presidential,
      aes(xmin = start, xmax = end, y = party, label = name)
    ) +
      geom_fit_text(grow = TRUE, size = 48) +
      geom_errorbar(alpha = 0.5)
  })

  expect_doppelganger("Box defined with xmin/xmax and ymin/ymax", {
    ggplot(
      presidential,
      aes(xmin = start, xmax = end, ymin = start, ymax = end, label = name)
    ) +
      geom_fit_text(grow = TRUE, size = 48)
  })
})

test_that("geom_fit_text() draws contrasting text", {
  expect_doppelganger("Contrasting text", {
    ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +
      geom_tile() +
      geom_fit_text(reflow = TRUE, grow = TRUE, contrast = TRUE, size = 48)
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

test_that("geom_fit_text() draws, grows and correctly places text in polar coordinates", {
  expect_doppelganger("Polar geom_fit_text() plot", {
    ggplot(
      gold,
      aes(label = line, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    ) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(min.size = 0)
  })

  expect_doppelganger("Polar plot with growing, non-fullheight", {
    ggplot(
      gold,
      aes(label = line, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    ) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(grow = TRUE, fullheight = FALSE, min.size = 0)
  })

  expect_doppelganger("Polar plot with growing, fullheight", {
    ggplot(
      gold,
      aes(label = line, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    ) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(grow = TRUE, fullheight = TRUE, min.size = 0)
  })

  expect_doppelganger("Polar plot with text placed at the top", {
    ggplot(
      gold,
      aes(label = line, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    ) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(place = "top", min.size = 0)
  })

  expect_doppelganger("Polar plot with text placed at the bottom", {
    ggplot(
      gold,
      aes(label = line, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    ) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(place = "bottom", min.size = 0)
  })
})


test_that("geom_fit_text() emits an error if you try to draw rich text in polar coordinates", {
  expect_error({
    p <- ggplot(
      gold,
      aes(label = line, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    ) +
      geom_rect(fill = "lightblue") +
      coord_polar(start = (pi / 2)) +
      geom_fit_text(min.size = 0, rich = TRUE)
    print(p)
  })
})


test_that("geom_fit_text() correctly applies width and height arguments in polar coordinates", {
  expect_doppelganger("Polar plot with height of 20 mm", {
    ggplot(gold) +
      coord_polar() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      geom_fit_text(
        aes(label = line, xmin = xmin, xmax = xmax, y = (ymin + ymax) / 2),
        min.size = 0,
        grow = TRUE,
        height = grid::unit(20, "mm")
      )
  })

  expect_doppelganger("Polar plot with height of 5 mm", {
    ggplot(gold) +
      coord_polar() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      geom_fit_text(
        aes(label = line, xmin = xmin, xmax = xmax, y = (ymin + ymax) / 2),
        min.size = 0,
        grow = TRUE,
        height = grid::unit(5, "mm")
      )
  })

  expect_doppelganger("Polar plot with width of 100 mm", {
    ggplot(gold) +
      coord_polar() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      geom_fit_text(
        aes(label = line, x = (xmin + xmax) / 2, ymin = ymin, ymax = ymax),
        min.size = 0,
        grow = TRUE,
        width = grid::unit(100, "mm")
      )
  })

  expect_doppelganger("Polar plot with width of 40 mm", {
    ggplot(gold) +
      coord_polar() +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      geom_fit_text(
        aes(label = line, x = (xmin + xmax) / 2, ymin = ymin, ymax = ymax),
        min.size = 0,
        grow = TRUE,
        width = grid::unit(40, "mm")
      )
  })
})

test_that("The flip argument works properly when drawing in polar coordinates", {
  expect_doppelganger("Polar plot with flip", {
    ggplot(
      gold,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = line)
    ) +
      coord_polar() +
      geom_rect() +
      geom_fit_text(min.size = 0, flip = TRUE, grow = TRUE)
  })

  expect_doppelganger("Polar plot with flip and shifted start", {
    ggplot(
      gold,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = line)
    ) +
      coord_polar(start = (pi / 2)) +
      geom_rect() +
      geom_fit_text(min.size = 0, flip = TRUE, grow = TRUE)
  })

  expect_doppelganger("Polar plot with flip and text in top left", {
    ggplot(
      gold,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = line)
    ) +
      coord_polar() +
      geom_rect() +
      geom_fit_text(min.size = 0, flip = TRUE, place = "topleft")
  })
})

# Logic-level test for the reflow engine. Unlike the vdiffr snapshots above, it
# inspects the returned grob directly, so it catches metric bugs that leave a
# visually plausible but incorrect result.

test_that("reflow_and_resize() never returns an unshrunk wrap that overflows the box (#47)", {

  # A label whose successive candidate wraps are non-monotone in rendered width
  # (mixed wide/narrow glyphs). This is what exposed the bug: the "widest wrap
  # that fits" filter could select a wrap that does not fit, and because that
  # branch returns early the text was never shrunk to compensate.
  label <- "MMMM ii MMMM iiiiiiiiii MM iii MMMMM WW i WWWW"
  text <- data.frame(
    label = label, size = 12, angle = 0, colour = "black", alpha = 1,
    family = "", fontface = 1, lineheight = 0.9, stringsAsFactors = FALSE
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())

  ydim <- 30

  # Sweep box widths across the range where candidate wrap widths diverge. The
  # exact trigger band is font-metric-dependent, so we assert the invariant over
  # a broad sweep rather than at one fragile point.
  for (xdim in seq(40, 70, by = 0.5)) {
    tg <- reflow_and_resize(
      text, reflow = TRUE, grow = FALSE, fullheight = FALSE,
      xdim = xdim, ydim = ydim, rich = FALSE
    )

    # If the font size is unchanged, no shrinking was applied, so the chosen
    # wrap must genuinely fit the box in both dimensions.
    if (isTRUE(all.equal(getfontsize(tg), text$size))) {
      dim <- tgDimensions(tg, FALSE, text$angle)
      expect_lte(dim$width, xdim + 1e-6)
      expect_lte(dim$height, ydim + 1e-6)
    }
  }
})

test_that("GeomFitText$setup_data() does not add a dead 'flip' column", {
  p <- ggplot(animals, aes(x = type, y = flies, label = animal)) +
    geom_fit_text()
  expect_false("flip" %in% names(layer_data(p)))
})

test_that("deprecated geom_grow_text()/geom_shrink_text() stubs are removed", {
  ns <- asNamespace("ggfittext")
  expect_false(exists("geom_grow_text", envir = ns, inherits = FALSE))
  expect_false(exists("geom_shrink_text", envir = ns, inherits = FALSE))
})

# Logic-level test for the min.size gate. A label that can only be fitted by
# shrinking below min.size must be dropped entirely, leaving no child grob. With
# grow = FALSE the 12 pt text is never enlarged, so a 200 pt floor is guaranteed
# to reject every label whatever the box size or font metrics.
test_that("labels that must shrink below min.size produce no child grob", {
  df <- data.frame(x = 1:3, y = 1:3, label = c("a", "b", "c"))
  p <- ggplot(df, aes(x, y, label = label)) + geom_fit_text(min.size = 200)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())

  drawn <- grid::makeContent(suppressWarnings(layer_grob(p)[[1]]))
  expect_length(drawn$children, 0)
})

# hjust is derived from the horizontal component of `place`, and manual
# hjust/vjust must override the derived defaults. The box is large enough that
# the label always draws (asserted before indexing, so a dropped label fails
# semantically rather than with a subscript error).
test_that("hjust follows place, and manual hjust/vjust override it", {
  df <- data.frame(xmin = 0, xmax = 10, ymin = 0, ymax = 10, label = "hi")
  child <- function(...) {
    p <- ggplot(
      df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = label)
    ) +
      geom_fit_text(...)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    drawn <- grid::makeContent(suppressWarnings(layer_grob(p)[[1]]))
    expect_length(drawn$children, 1)
    drawn$children[[1]]
  }

  # hjust follows the horizontal component of `place`. vjust is not
  # place-derived — vertical position is handled by the placement maths, so it
  # always defaults to 0.5.
  hjusts <- c(
    topleft = 0, left = 0, bottomleft = 0,
    top = 0.5, centre = 0.5, bottom = 0.5,
    topright = 1, right = 1, bottomright = 1
  )
  for (place in names(hjusts)) {
    tg <- child(place = place)
    expect_equal(tg$hjust, hjusts[[place]], info = place)
    expect_equal(tg$vjust, 0.5, info = place)
  }

  # Explicit hjust/vjust take precedence over the place-derived defaults.
  overridden <- child(place = "centre", hjust = 0, vjust = 1)
  expect_equal(overridden$hjust, 0)
  expect_equal(overridden$vjust, 1)
})

# The `outside` path (R/geom_fit_text.R draw of below-min.size labels) is the
# flagship behaviour of geom_bar_text() but lives in the shared engine. A label
# too big for its box but with room outside it should be dropped when
# outside = FALSE and rescued when outside = TRUE.
test_that("outside = TRUE draws below-min.size labels outside the box", {
  # A wide but extremely short box, with ample room above it in the panel.
  df <- data.frame(xmin = 0, xmax = 100, ymin = 0, ymax = 0.1, label = "hi")
  draw <- function(outside) {
    p <- ggplot(
      df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = label)
    ) +
      geom_fit_text(place = "top", outside = outside, min.size = 8) +
      ylim(0, 10)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    grid::makeContent(suppressWarnings(layer_grob(p)[[1]]))$children
  }

  expect_length(draw(FALSE), 0)
  expect_length(draw(TRUE), 1)
})

# A valid formatter is applied to each label during the build step.
test_that("geom_fit_text() applies a valid formatter to the labels", {
  p <- ggplot(
    data.frame(x = 1:2, y = 1:2, label = c("a", "b")),
    aes(x, y, label = label)
  ) +
    geom_fit_text(formatter = function(x) paste0(x, "!"))
  expect_equal(layer_data(p)$label, c("a!", "b!"))
})

# An invalid formatter must be rejected during the build step, before any
# drawing. A non-function is caught by the geom's own check; a formatter that
# returns the wrong type or length is rejected by vapply's template enforcement
# before that check. Either way a bad formatter never reaches drawing.
test_that("geom_fit_text() rejects invalid formatters", {
  base <- ggplot(
    data.frame(x = 1:2, y = 1:2, label = c("a", "b")),
    aes(x, y, label = label)
  )

  # Not a function at all (the geom's own check).
  expect_error(
    ggplot_build(base + geom_fit_text(formatter = "nope")),
    "must be a function"
  )

  # Returns the wrong type.
  expect_error(ggplot_build(base + geom_fit_text(formatter = function(x) 1)))

  # Returns more than one value per label.
  expect_error(
    ggplot_build(base + geom_fit_text(formatter = function(x) c(x, x)))
  )
})
