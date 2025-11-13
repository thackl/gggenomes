# Draw feats

`geom_feat()` allows the user to draw (additional) features to the
plot/graph. For example, specific regions within a sequence (e.g.
transposons, introns, mutation hotspots) can be highlighted by color,
size, etc..

## Usage

``` r
geom_feat(
  mapping = NULL,
  data = feats(),
  stat = "identity",
  position = "pile",
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

  feat_layout: Uses first data frame stored in the `feats` track by
  default.

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

  describes how the position of different plotted features are adjusted.
  By default it uses `"pile"`, but different ggplot2 position
  adjustments, such as `"identity` or `"jitter"` can be used as well.

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

A ggplot2 layer with features.

## Details

`geom_feat` uses
[`ggplot2::geom_segment`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
under the hood. As a result, different aesthetics such as *alpha*,
*linewidth*, *color*, etc. can be called upon to modify the
visualization of the data.

*By default, the function uses the first feature track.*

## Examples

``` r
# Plotting data from the feats' track with adjusted linewidth and color
gggenomes(seqs = emale_seqs, feats = emale_ngaros) +
  geom_seq() +
  geom_feat(linewidth = 5, color = "darkred")


# Geom_feat can be called several times as well, when specified what data should be used
gggenomes(seqs = emale_seqs, feats = list(emale_ngaros, emale_tirs)) +
  geom_seq() +
  geom_feat(linewidth = 5, color = "darkred") + # uses first feature track
  geom_feat(data = feats(emale_tirs))


# Additional notes to feats can be added with functions such as: geom_feat_note / geom_feat_text
gggenomes(seqs = emale_seqs, feats = list(emale_ngaros, emale_tirs)) +
  geom_seq() +
  geom_feat(color = "darkred") +
  geom_feat(data = feats(emale_tirs), color = "darkblue") +
  geom_feat_note(data = feats(emale_ngaros), label = "repeat region", size = 4)


# Different position adjustments with a simple dataset
exampledata <- tibble::tibble(
  seq_id = c(rep("A", 3), rep("B", 3), rep("C", 3)),
  start = c(0, 30, 15, 40, 80, 20, 30, 50, 70),
  end = c(30, 90, 60, 60, 100, 80, 60, 90, 120)
)

gggenomes(feats = exampledata) +
  geom_feat(position = "identity", alpha = 0.5, linewidth = 0.5) +
  geom_bin_label()
#> No seqs provided, inferring seqs from feats
```
