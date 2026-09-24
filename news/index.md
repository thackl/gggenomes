# Changelog

## gggenomes 1.2.0

- added an
  [`axis_scalebar()`](https://thackl.github.io/gggenomes/reference/axis_scalebar.md)
  as default x-axis and an optional
  [`geom_scalebar()`](https://thackl.github.io/gggenomes/reference/axis_scalebar.md).
- revamped handling of items outside or on the edge of the plotting
  area. See
  [`vignette("marginal")`](https://thackl.github.io/gggenomes/articles/marginal.md).
- made a stand-alone, reproducible tutorial including raw data out of
  [`vignette("emales")`](https://thackl.github.io/gggenomes/articles/emales.md).

## gggenomes 1.1.3

CRAN release: 2026-02-23

- make “id” global variable for compatibility with dplyr 1.2.0

## gggenomes 1.1.2

CRAN release: 2025-11-14

- new [`align()`](https://thackl.github.io/gggenomes/reference/align.md)
  function to arrange genomes relative to genes/regions of interest
  ([\#204](https://github.com/thackl/gggenomes/issues/204))
- new
  [`geom_link_curved()`](https://thackl.github.io/gggenomes/reference/geom_link.md)
  for bezier-style connectores
  ([\#104](https://github.com/thackl/gggenomes/issues/104))
- bugfix for genbank parser
  ([\#203](https://github.com/thackl/gggenomes/issues/203))
- fix of a lot of lifecycle warnings

## gggenomes 1.1.0

- Updated for **ggplot2 4.0** layout changes — restores full
  compatibility
- Requires **R ≥ 4.1.0** (native pipe `|>` support)
- Replaced deprecated `size` aesthetic in line geoms with `linewidth`
- CI configuration refreshed for multi-platform testing
- Examples guarded for missing optional packages in Windows oldrel
  builds (**Hmisc**, **ggtree**)
- CITATION file updated
  ([`bibentry()`](https://rdrr.io/r/utils/bibentry.html)/[`person()`](https://rdrr.io/r/utils/person.html))
- Minor typo and code clean-ups
