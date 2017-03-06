# Installation

Install ggplot2 and devtools if you don't have them already.

``` r
install.packages("ggplot2")
install.packages("devtools")
```

Install ggfittext.

``` r
library(devtools)
install_github('wilkox/ggfittext')
```

# Walkthrough

Sometimes you want to draw some text in ggplot2 so that it doesn't spill outside
a bounding box. For example:

```{r}
library(ggfittext)
flyers
#>         vehicle xmin xmax ymin ymax     class  x    y
#> 1   light plane   10   20   10   20     plane 15 15.0
#> 2     jumbo jet   20   90   75   95     plane 55 85.0
#> 3 space shuttle   80   90   15   50 spaceship 85 32.5
#> 4  dyson sphere   25   75   25   60 spaceship 50 42.5
ggplot(flyers) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = class)) +
  geom_text(aes(label = vehicle, x = x, y = y))
```

![Text drawn with `geom_text`](vignettes/geom_text.png)

ggfittext provides a new geom, `geom_fit_text`, that will shrink text when
needed to fit a bounding box:

```{r}
ggplot(flyers, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label =
                   vehicle, fill = class)) +
  geom_rect() +
  geom_fit_text()
```

![Text drawn with `geom_fit_text`](vignettes/geom_fit_text.png)

You can define the x position and dimension of the box with ‘xmin’ and ‘xmax’,
or alternatively with ‘x’ and ‘width’ (width is given in millimetres). Likewise,
the y dimension and position can be defined with ‘ymin’ and ‘ymax’ or with ‘y’
and ‘height’. This can be useful when drawing on a discrete axis.

You can specify where in the bounding box to place the text, and hide text that
would be shrunk below a minimum size:

```{r}
ggplot(flyers, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label =
                   vehicle, fill = class)) +
  geom_rect() +
  geom_fit_text(place = "topleft", min.size = 8)
```

![Text drawn with `geom_fit_text` and arguments](vignettes/geom_fit_text_args.png)

The obvious set of placements (‘topleft’, ‘top’, ‘topright’...) are supported,
as well as the default ‘centre’. ‘vjust’ and ‘hjust’ are set automatically.

With the `grow = T` argument, text will be grown as well as shrunk to fit the
box:

```{r}
ggplot(flyers, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label =
                   vehicle, fill = class)) +
  geom_rect() +
  geom_fit_text(grow = T)
```

![Text drawn with `geom_fit_text` and arguments including `grow = T`](vignettes/geom_fit_text_grow.png)
