# Changelog

## gggenomes 1.1.1

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
