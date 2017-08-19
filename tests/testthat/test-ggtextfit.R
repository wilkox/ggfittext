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
  expect_error( {
    ggplot2::ggplot(testdata, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = vehicle, colour = class)) + geom_fit_text(padding.x = grid::unit(1, "lines"), padding.y = grid::unit(3, "mm"), min.size = 2, place = "topright")
  } , NA)
  expect_error( {
    ggplot2::ggplot(testdata, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = vehicle, colour = class)) + geom_fit_text(padding.x = grid::unit(1, "lines"), padding.y = grid::unit(3, "mm"), min.size = 2, place = "topright")
  } , NA)
})

test_that("missing aesthetics and bad options don't work", {
  expect_error( {
    p <- ggplot2::ggplot(testdata, ggplot2::aes(x = xmin, y = ymax, label = vehicle, colour = class))
    p <- p + geom_fit_text(padding.x = 12, min.size = "nottoobigplease", place = "near the top")
    print(p)
  })

})

context("growing text")

test_that("simple plots and options work", {
  expect_error( {
    ggplot2::ggplot(testdata, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = vehicle, colour = class)) + geom_fit_text(padding.x = grid::unit(1, "lines"), padding.y = grid::unit(3, "mm"), min.size = 2, place = "bottom", grow = T)
  } , NA)
  expect_error( {
    ggplot2::ggplot(testdata, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = vehicle, colour = class)) + geom_fit_text(padding.x = grid::unit(1, "lines"), padding.y = grid::unit(3, "mm"), min.size = 2, place = "bottom", grow = T)
  } , NA)
})

context("reflowing text")

test_that("simple plots and options work", {
  expect_error( {
    ggplot2::ggplot(testdata, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = vehicle, colour = class)) + geom_fit_text(padding.x = grid::unit(1, "lines"), padding.y = grid::unit(3, "mm"), min.size = 2, place = "bottom", grow = T, reflow = T)
  } , NA)
  expect_error( {
    ggplot2::ggplot(testdata, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = vehicle, colour = class)) + geom_fit_text(padding.x = grid::unit(1, "lines"), padding.y = grid::unit(3, "mm"), min.size = 2, place = "bottom", reflow = T, grow = T)
  } , NA)
})

test_that("missing aesthetics and bad options don't work", {
  expect_error( {
    p <- ggplot2::ggplot(testdata, ggplot2::aes(x = xmin, y = ymax, label = vehicle, colour = class))
    p <- p + geom_fit_text(padding.x = 12, min.size = "nottoobigplease", place = "near the top")
    print(p)
  })

})
