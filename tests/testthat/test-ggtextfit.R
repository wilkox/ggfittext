testdata <- data.frame(
  vehicle = c("light plane", "jumbo jet", "space shuttle"),
  xmin = c(10, 20, 80),
  xmax = c(20, 60, 90),
  ymin = c(10, 75, 15),
  ymax = c(20, 95, 50),
  class = c("plane", "plane", "spaceship")
)

context("geom_shrink_text")

test_that("simple plots and options work", {
  expect_error( {
    ggplot(testdata, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = vehicle, colour = class)) + geom_shrink_text(padding.x = unit(1, "lines"), padding.y = unit(3, "mm"), min.size = 2, place = "topright")
  } , NA)
  expect_error( {
    ggplot(testdata, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = vehicle, colour = class)) + geom_shrink_text(padding.x = unit(1, "lines"), padding.y = unit(3, "mm"), min.size = 2, place = "topright", reflow = T)
  } , NA)
})

test_that("missing aesthetics and bad options don't work", {
  expect_error( {
    p <- ggplot(testdata, aes(x = xmin, y = ymax, label = vehicle, colour = class))
    p <- p + geom_shrink_text(padding.x = 12, min.size = "nottoobigplease", place = "near the top")
    print(p)
  })

})

context("geom_grow_text")

test_that("simple plots and options work", {
  expect_error( {
    ggplot(testdata, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = vehicle, colour = class)) + geom_grow_text(padding.x = unit(1, "lines"), padding.y = unit(3, "mm"), min.size = 2, place = "bottom")
  } , NA)
  expect_error( {
    ggplot(testdata, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = vehicle, colour = class)) + geom_grow_text(padding.x = unit(1, "lines"), padding.y = unit(3, "mm"), min.size = 2, place = "bottom", reflow = T)
  } , NA)
})

test_that("missing aesthetics and bad options don't work", {
  expect_error( {
    p <- ggplot(testdata, aes(x = xmin, y = ymax, label = vehicle, colour = class))
    p <- p + geom_grow_text(padding.x = 12, min.size = "nottoobigplease", place = "near the top")
    print(p)
  })

})
