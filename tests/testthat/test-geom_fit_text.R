library(ggplot2)
grid::current.viewport()
testdata <- data.frame(
  vehicle = c("light plane", "jumbo jet", "space shuttle"),
  xmin = c(10, 20, 80),
  xmax = c(20, 60, 90),
  ymin = c(10, 75, 15),
  ymax = c(20, 95, 50),
  class = c("plane", "plane", "spaceship")
)
z <- data.frame(x = letters[1:5], y = 0:4, lb = 3)

context("shrinking text")

test_that("simple plots and options do not produce errors", {
  expect_silent( {
    p <- ggplot2::ggplot(testdata, ggplot2::aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      label = vehicle,
      colour = class
    )) + 
      geom_fit_text(
        padding.x = grid::unit(1, "lines"),
        padding.y = grid::unit(3, "mm"),
        min.size = 2,
        place = "topright"
      )
    print(p)
  })
  expect_silent( {
    p <- ggplot2::ggplot(testdata, ggplot2::aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      label = vehicle,
      colour = class
    )) + 
      geom_fit_text(
        padding.x = grid::unit(1, "lines"),
        padding.y = grid::unit(3, "mm"),
        min.size = 2,
        place = "topright"
      ) 
    print(p)
  })
})

test_that("missing aesthetics and bad options don't work", {
  expect_error( {
    p <- ggplot2::ggplot(testdata, ggplot2::aes(
      x = xmin,
      y = ymax,
      label = vehicle,
      colour = class
    ))
    p <- p + geom_fit_text(
      padding.x = 12,
      min.size = "nottoobigplease",
      place = "near the top"
    )
    print(p)
  })
})

context("growing text")

test_that("simple plots and options do not produce errors", {
  expect_silent( {
    p <- ggplot2::ggplot(testdata, ggplot2::aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      label = vehicle,
      colour = class
    )) + 
      geom_fit_text(
        padding.x = grid::unit(1, "lines"),
        padding.y = grid::unit(3, "mm"),
        min.size = 2,
        place = "bottom",
        grow = T
      )
    print(p)
  })
  expect_silent( {
    p <- ggplot2::ggplot(testdata, ggplot2::aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      label = vehicle,
      colour = class
    )) + 
      geom_fit_text(
        padding.x = grid::unit(1, "lines"),
        padding.y = grid::unit(3, "mm"),
        min.size = 2,
        place = "bottom",
        grow = T
      )
    print(p)
  })
})

context("reflowing text")

test_that("simple plots and options do not produce errors", {
  expect_silent( {
    p <- ggplot2::ggplot(testdata, ggplot2::aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      label = vehicle,
      colour = class
    )) + 
      geom_fit_text(
        padding.x = grid::unit(1, "lines"),
        padding.y = grid::unit(3, "mm"),
        min.size = 2,
        place = "bottom",
        grow = T,
        reflow = T
      )
    print(p)
  })
  expect_silent( {
    p <- ggplot2::ggplot(testdata, ggplot2::aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      label = vehicle,
      colour = class
    )) + 
      geom_fit_text(
        padding.x = grid::unit(1, "lines"),
        padding.y = grid::unit(3, "mm"),
        min.size = 2,
        place = "bottom",
        reflow = T,
        grow = T
      )
    print(p)
  })
})

test_that("missing aesthetics and bad options produce errors", {
  expect_error( {
    p <- ggplot2::ggplot(testdata, ggplot2::aes(
      x = xmin,
      y = ymax,
      label = vehicle,
      colour = class
    ))
    p <- p + geom_fit_text(
      padding.x = 12,
      min.size = "nottoobigplease",
      place = "near the top"
    )
    print(p)
  })
})

testdata2 <- data.frame(
  testdata,
  x = (testdata$xmin + testdata$xmax) / 2,
  y = (testdata$ymin + testdata$ymax) / 2,
  width = testdata$xmax - testdata$xmin,
  height = testdata$ymax - testdata$ymin
)

test_that("numeric 'width' and 'height' parameters are understood", {
  expect_silent( {
    p <- ggplot2::ggplot(testdata2, ggplot2::aes(
      x = x,
      y = y,
      label = vehicle
    )) + 
      ggplot2::geom_tile(width = 10, height = 20, fill = "gray") + 
      geom_fit_text(width = 10, height = 20)
    print(p)
  })
  expect_silent( {
    p <- ggplot2::ggplot(testdata2, ggplot2::aes(x = x, y = y, label = vehicle)) + 
      ggplot2::geom_tile(width = 10, height = 20, fill = "gray") + 
      geom_fit_text(width = 10, height = 20, reflow = TRUE)
    print(p)
  })
})

context("formatting text")

test_that("a `formatter` argument is accepted", {
  expect_silent( {
    library(ggplot2)
    wiki <- function(x) { paste0(x, " (citation needed)") }

    yeats <- data.frame(
      xmin = c(0, 4, 6, 4, 4, 5, 5.5, 5,   5,    5.25, 5.25),
      xmax = c(4, 8, 8, 6, 5, 6, 6,   5.5, 5.25, 5.5,  5.5),
      ymin = c(0, 4, 0, 0, 2, 3, 2,   2,   2.5,  2.75, 2.5),
      ymax = c(8, 8, 4, 2, 4, 4, 3,   2.5, 3,    3,    2.75),
      label = c("Turning", "and", "turning", "in", "the", "widening", "gyre", "the",
                "falcon", "cannot", "hear"),
      angle = sample(0:360, 11)
    )

    p <- ggplot(yeats, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label =
                      label, angle = angle)) +
      geom_rect(colour = "black") +
      geom_fit_text(grow = T, min.size = 0, formatter = wiki)
    print(p)
  } )
} )

context("blank labels")

test_that("a blank label should not result in an error", {
  expect_silent( {
    presidential$name[1] <- ""
    p <- ggplot(presidential, 
       aes(ymin = start, ymax = end, label = name, x = party)) +
        geom_fit_text(grow = TRUE)
    print(p)
  } )
})

context("box limits out of plot limits")

test_that("box limits outside of plot limits should produce a warning", {
  expect_warning( {
    p <- ggplot(z, aes(x = x, y = y, label = lb)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      ylim(0.5, 3) + 
      geom_fit_text()
    print(p)
  }, "box limits were outside plot limits")

  expect_warning( {
    p <- ggplot(z, aes(x = x, y = y, label = lb)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      ylim(-0.1, 6) + 
      geom_fit_text()
    print(p)
  }, "box limits were outside plot limits")

  expect_warning( {
    p <- ggplot(z, aes(x = y, y = x, label = lb)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      xlim(-0.1, 6) + 
      geom_fit_text()
    print(p)
  }, "box limits were outside plot limits")
} )
