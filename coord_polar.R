load_all()
library(ggplot2)
testdata <- data.frame(
  vehicle = c("light plane", "jumbo jet", "space shuttle"),
  xmin = c(10, 20, 80),
  xmax = c(20, 60, 90),
  ymin = c(10, 75, 15),
  ymax = c(20, 95, 50),
  class = c("plane", "plane", "spaceship")
)
ggplot(testdata, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = vehicle, colour = class)) + 
  geom_fit_text(padding.x = grid::unit(1, "lines"), padding.y = grid::unit(3, "mm"), min.size = 2, place = "bottom", grow = T, reflow = T)
