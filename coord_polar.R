library(tidyverse)

d <- data.frame(
  word = c("The", "falcon", "cannot", "hear", "the", "falconer"),
  xmin = c( 0.1,   0.3,      0.6,      0.8,    0.5,   0.2),
  xmax = c( 0.2,   0.55,      0.8,      1.0,    0.7,   0.6),
  ymin = c( 0.65,  0.45,     0.55,     0.4,    0.25,  0.1),
  ymax = c( 0.95,  0.8,      0.95,     0.5,    0.35,  0.2)
)

load_all()
ggplot(d, aes(label = word, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1)) +
  geom_rect(fill = "lightblue") +
  coord_polar(start = (pi / 2)) +
  geom_fit_text(
    aes(x = 1, y = 1),
    grow = TRUE,
    padding.x = grid::unit(2, "mm"),
    padding.y = grid::unit(2, "mm"),
    place = "bottom",
    min.size = 0
  )
