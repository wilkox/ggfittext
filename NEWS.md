# v0.5.0

## Minor changes

- Change R and grid dependency to 3.1

## Bug fixes

- Fix typo in README
- Remove README.html so README renders correctly on GitHub

# v0.4.3

## Minor changes
- Style change for CRAN submission.
- Add example to `geom_fit_text()` documentation.

# v0.4.2

## Minor changes
- Release version for CRAN submission.

## Bug fixes
- Tidy up some namespace issues (thanks to @JohnsonHsieh)
- Rewrite documentation
- Add example to README, edit and produce vignette
- Misc. minor fixes to pass R CMD check

## Minor changes
- Deprecate `geom_shrink_text()` and `geom_grow_text()`

# v0.4.1

## Bug fixes
- Fix wide characters causing problems in non-UTF-8 locales.

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
