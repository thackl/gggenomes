# Draw feat/link labels

These `geom_..._label()` functions able the user to plot labels/text at
individual features and/or links. Users have to indicate how to label
the features/links by specifying `label = ...` or `aes(label = ...`

Position of labels can be adjusted with arguments such as `vjust`,
`hjust`, `angle`, `nudge_y`, etc. Also check out
[`geom_bin_label()`](https://thackl.github.io/gggenomes/reference/geom_bin_label.md),
[`geom_seq_label()`](https://thackl.github.io/gggenomes/reference/geom_seq_label.md)
or
[`geom_feat_text()`](https://thackl.github.io/gggenomes/reference/geom_feat_text.md)
given their resemblance.

## Usage

``` r
geom_gene_label(
  mapping = NULL,
  data = genes(),
  angle = 45,
  hjust = 0,
  nudge_y = 0.1,
  size = 6,
  ...
)

geom_feat_label(
  mapping = NULL,
  data = feats(),
  angle = 45,
  hjust = 0,
  nudge_y = 0.1,
  size = 6,
  ...
)

geom_link_label(
  mapping = NULL,
  data = links(),
  angle = 0,
  hjust = 0.5,
  vjust = 0.5,
  size = 4,
  repel = FALSE,
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

- angle:

  Defines the angle in which the text will be placed. \*Note

- hjust:

  Moves the text horizontally

- nudge_y:

  Moves the text vertically an entire contig/sequence. (e.g.
  `nudge_y = 1` places the text to the contig above)

- size:

  of the label

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

- vjust:

  Moves the text vertically

- repel:

  use ggrepel to avoid overlaps

## Value

Gene labels are added as a text layer/component to the plot.

## Details

These labeling functions use
[`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
under the hood. Any changes to the aesthetics of the text can be
performed in a ggplot2 manner.
