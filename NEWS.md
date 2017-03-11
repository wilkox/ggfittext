# v0.4

## Major changes
- Add ‘reflow’ option to `geom_fit_text`.

## Bug fixes
- Fix height calculation on text with ascenders/descenders.

# v0.3

## Major changes
- Much faster text resizing algorithm
- Rename arguments: ‘discrete.width’ and ‘discrete.height’ to ‘width’ and
  ‘height’; ‘fill.text’ to ‘grow’

# v0.2

## Major changes
- `geom_shrink_text` and `geom_fill_text` replaced with single `geom_fit_text`
  with `fill_text` option. (The old geoms still work, but as wrappers for
  `geom_fit_text`).
- Add support for discrete axes with new `discrete.height` and `discrete.width`
  options.

## Bug fixes
- Font sizes now correctly and consistently represented as point sizes.

# v0.1
First release!
