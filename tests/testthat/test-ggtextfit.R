testdata <- data.frame(
  vehicle = c("light plane", "jumbo jet", "space shuttle"),
  xmin = c(10, 20, 80),
  xmax = c(20, 60, 90),
  ymin = c(10, 75, 15),
  ymax = c(20, 95, 50),
  class = c("plane", "plane", "spaceship")
)

context("shrinking text")

test_that("simple plots and options work", {
  expect_silent( {
    ggplot2::ggplot(testdata, ggplot2::aes(
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
  })
  expect_silent( {
    ggplot2::ggplot(testdata, ggplot2::aes(
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
      ) })
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

test_that("simple plots and options work", {
  expect_silent( {
    ggplot2::ggplot(testdata, ggplot2::aes(
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
  })
  expect_silent( {
    ggplot2::ggplot(testdata, ggplot2::aes(
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
  })
})

context("reflowing text")

test_that("simple plots and options work", {
  expect_silent( {
    ggplot2::ggplot(testdata, ggplot2::aes(
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
  })
  expect_silent( {
    ggplot2::ggplot(testdata, ggplot2::aes(
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

testdata2 <- data.frame(
  testdata,
  x = (testdata$xmin + testdata$xmax) / 2,
  y = (testdata$ymin + testdata$ymax) / 2,
  width = testdata$xmax - testdata$xmin,
  height = testdata$ymax - testdata$ymin
)

test_that("numeric 'width' and 'height' parameters are understood", {
  expect_silent( {
    ggplot2::ggplot(testdata2, ggplot2::aes(
      x = x,
      y = y,
      label = vehicle
    )) + 
      ggplot2::geom_tile(width = 10, height = 20, fill = "gray") + 
      geom_fit_text(width = 10, height = 20)
  })
  expect_silent( {
    ggplot2::ggplot(testdata2, ggplot2::aes(x = x, y = y, label = vehicle)) + 
      ggplot2::geom_tile(width = 10, height = 20, fill = "gray") + 
      geom_fit_text(width = 10, height = 20, reflow = TRUE)
  })
})
