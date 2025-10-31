# gggenomes 1.1.1

* new `align()` function to arrange genomes relative to genes/regions of interest (#204)
* new `geom_link_curved()` for bezier-style connectores (#104)
* bugfix for genbank parser (#203)
* fix of a lot of lifecycle warnings 

# gggenomes 1.1.0

* Updated for **ggplot2 4.0** layout changes — restores full compatibility
* Requires **R ≥ 4.1.0** (native pipe `|>` support)
* Replaced deprecated `size` aesthetic in line geoms with `linewidth`
* CI configuration refreshed for multi-platform testing
* Examples guarded for missing optional packages in Windows oldrel builds (**Hmisc**, **ggtree**)
* CITATION file updated (`bibentry()`/`person()`)
* Minor typo and code clean-ups
