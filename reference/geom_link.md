# Draw links between genomes

Draw connections between genomes, such as genome/gene/protein alignments
and gene/protein clusters. `geom_link()` and `geom_link_curved()` create
filled polygons between regions, `geom_link_line()` a single connecting
line.

Note that by default only links between adjacent genomes are computed
and shown. To compute and show all links between all genomes, set
`gggenomes(..., adjacent_only=FALSE)`.

## Usage

``` r
geom_link(
  mapping = NULL,
  data = links(),
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  offset = 0.15,
  curve = NA,
  ...
)

geom_link_curved(
  mapping = NULL,
  data = links(),
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  offset = 0.15,
  curve = 10,
  ...
)

geom_link_line(
  mapping = NULL,
  data = links(),
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- offset:

  distance between seq center and link start. Use two values
  `c(<offset_top>, <offset_bottom>)` for different top and bottom
  offsets

- curve:

  curvature of the link. If `NA` or `0`, the link edges will be
  straight. For curved links, higher values lead to stronger curvature.
  Typical values are between `5` and `15`.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

## Value

A ggplot2 layer with links.

A ggplot2 layer with links.

A ggplot2 layer with links.

## Details

The function calls upon the data stored within the `link` track. Data
frames added to this track have `seq_id` and `seq_id2` as required
variables. Optional and recommended variables include `start`, `start2`,
`end`, `end2`, `bin_id`, `bin_id2` and `strand`.

Note, when start/end is not specified, links will be created between the
entire contigs of `seq_id` and `seq_id2`.

## Examples

``` r
p0 <- gggenomes(seqs = emale_seqs, links = emale_ava) + geom_seq()

# default links
p1 <- p0 + geom_link()

# change offset from seqs and color
p2 <- p0 + geom_link(aes(fill = de, color = de), offset = 0.05) +
  scale_fill_viridis_b() + scale_colour_viridis_b()

# combine with flip
p3 <- p0 |> flip(3, 4, 5) +
  geom_link()

# compute & show all links among all genomes
# usually not useful and not recommended for large dataset
p4 <- gggenomes(links = emale_ava, adjacent_only = FALSE) + geom_link()
#> No seqs or feats provided, inferring seqs from links

library(patchwork) # combine plots in one figure
p1 + p2 + p3 + p4 + plot_layout(nrow = 1)


q0 <- gggenomes(emale_genes, emale_seqs) |>
  add_clusters(emale_cogs) +
  geom_seq() + geom_gene()
#> Joining with `by = join_by(feat_id)`

qq <- 
# link gene clusters with polygon
q0 + geom_link(aes(fill = cluster_id)) +
# link with curved polygons (bezier-like)
q0 + geom_link_curved(aes(fill = cluster_id)) +
# link gene clusters with lines
q0 + geom_link_line(aes(color = cluster_id))

qq + plot_layout(nrow = 1, guides = "collect")
```
