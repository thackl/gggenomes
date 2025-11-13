# Decorate truncated sequences

`geom_seq_break()` adds decorations to the ends of truncated sequences.
These could arise from zooming onto sequence loci with
[`focus()`](https://thackl.github.io/gggenomes/reference/focus.md), or
manually annotating sequences with start \> 1 and/or end \< length.

## Usage

``` r
geom_seq_break(
  mapping_start = NULL,
  mapping_end = NULL,
  data_start = seqs(start > 1),
  data_end = seqs(end < length),
  label = "/",
  size = 4,
  hjust = 0.75,
  family = "sans",
  stat = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
```

## Arguments

- mapping_start:

  optional start mapping

- mapping_end:

  optional end mapping

- data_start:

  seq_layout of sequences for which to decorate the start. default:
  `seqs(start >1)`

- data_end:

  seq_layout of sequences for which to decorate the end. default:
  `seqs(end < length)`

- label:

  the character to decorate ends with. Provide two values for different
  start and end decorations, e.g. `label=c("]", "[")`.

- size:

  of the text

- hjust:

  Moves the text horizontally

- family:

  font family of the text

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

A ggplot2 layer with sequence breaks.

## Examples

``` r
# decorate breaks created with focus()
gggenomes(emale_genes, emale_seqs) |>
  focus(.expand = 1e3, .max_dist = 1e3) +
  geom_seq() + geom_gene() +
  geom_seq_break()
#> Showing 10 loci with the following size distribution
#> • min: 3853
#> • q25: 9284
#> • med: 12695
#> • q75: 18007
#> • max: 21311


# customize decorations
gggenomes(emale_genes, emale_seqs) |>
  focus(.expand = 1e3, .max_dist = 1e3) +
  geom_seq() + geom_gene() +
  geom_seq_break(label = c("[", "]"), size = 3, color = "#1b9e77")
#> Showing 10 loci with the following size distribution
#> • min: 3853
#> • q25: 9284
#> • med: 12695
#> • q75: 18007
#> • max: 21311


# decorate manually truncated sequences
s0 <- tibble::tribble(
  # start/end define regions, i.e. truncated contigs
  ~bin_id, ~seq_id, ~length, ~start, ~end,
  "complete_genome", "chromosome_1_long_trunc_2side", 1e5, 1e4, 2.1e4,
  "fragmented_assembly", "contig_1_trunc_1side", 1.3e4, .9e4, 1.3e4,
  "fragmented_assembly", "contig_2_short_complete", 0.3e4, 1, 0.3e4,
  "fragmented_assembly", "contig_3_trunc_2sides", 2e4, 1e4, 1.4e4
)

l0 <- tibble::tribble(
  ~seq_id, ~start, ~end, ~seq_id2, ~start2, ~end2,
  "chromosome_1_long_trunc_2side", 1.1e4, 1.4e4,
  "contig_1_trunc_1side", 1e4, 1.3e4,
  "chromosome_1_long_trunc_2side", 1.4e4, 1.7e4,
  "contig_2_short_complete", 1, 0.3e4,
  "chromosome_1_long_trunc_2side", 1.7e4, 2e4,
  "contig_3_trunc_2sides", 1e4, 1.3e4
)

gggenomes(seqs = s0, links = l0) +
  geom_seq() + geom_link() +
  geom_seq_label(nudge_y = -.05) +
  geom_seq_break()
```
