# ggfittext 0.8.1

## Minor changes

- Add 'vdiffr' test cases

## Bug fixes

- Fix bug where a blank label causes an error

# ggfittext 0.8.0

## Major changes

- New `geom_bar_text()` convenience function for labelling of bars in bar plots
- New `contrast` argument for automatically contrasting a background fill
  colour
- New `fullheight` argument for including or excluding descenders in text size
- Faster for many common uses
- New `hjust` and `vjust` arguments that can be controlled independent of
  `place`
- New `outside` argument to allow text that doesn't fit inside the box to be
  teleported outside of it 

## Minor changes

- `padding.x` and `padding.y` now both default to 1 mm

## Bug fixes

- When reflowing text, calculation of the best aspect ratio now takes into
  account the output device's aspect ratio
- When reflowing text, calculation of the best aspect ratio no longer refuses
  to entertain the possibility that the original text might have the best ratio
- Angled text is now placed correctly

# ggfittext 0.7.0

## Major changes

- Add support for automatic sizing of text on discrete (categorical) axes
- Fully deprecate 'width' and 'height' as aesthetics
- Increase required R version to 3.2.3

## Minor changes

- Remove default values for 'width' and 'height' parameters
- Add new `formatter` argument to `geom_fit_text()`.
- Add visual tests with vdiffr.

## Bug fixes

- Fix tests that use `expect_silent()` and draw plots.

# ggfittext 0.6.0

## Major changes

- Change `width` and `height` from aesthetics to arguments.
- Allow `width` and `height` arguments to be provided in native units (thanks
  to @corybrunson).
- Add support for `position = "stack"`.

## Minor changes

- Miscellaneous code quality improvements.

## Bug fixes

- Fix typo in DESCRIPTION.

# ggfittext 0.5.0

## Minor changes

- Change R and grid dependency to 3.1.

## Bug fixes

- Fix typo in README.
- Remove README.html so README renders correctly on GitHub.

# ggfittext 0.4.3

## Minor changes

- Style change for CRAN submission.
- Add example to `geom_fit_text()` documentation.

# ggfittext 0.4.2

## Minor changes

- Release version for CRAN submission.

## Bug fixes

- Tidy up some namespace issues (thanks to @JohnsonHsieh).
- Rewrite documentation.
- Add example to README, edit and produce vignette.
- Misc. minor fixes to pass R CMD check.

## Minor changes

- Deprecate `geom_shrink_text()` and `geom_grow_text()`.

# ggfittext 0.4.1

## Bug fixes

- Fix wide characters causing problems in non-UTF-8 locales.

# ggfittext 0.4

## Major changes

- Add 'reflow' option to `geom_fit_text`.

## Bug fixes

- Fix height calculation on text with ascenders/descenders.

# ggfittext 0.3

## Major changes

- Much faster text resizing algorithm.
- Rename arguments: 'discrete.width' and 'discrete.height' to 'width' and
  'height'; 'fill.text' to 'grow'.

# ggfittext 0.2

## Major changes

- `geom_shrink_text()` and `geom_fill_text()` replaced with single
  `geom_fit_text()` with 'fill_text' option. (The old geoms still work, but as
  wrappers for `geom_fit_text()`).
- Add support for discrete axes with new `discrete.height` and `discrete.width`
  options.

## Bug fixes

- Font sizes now correctly and consistently represented as point sizes.

# ggfittext 0.1

First release!
