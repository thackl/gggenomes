# Draw seq labels

This function will put labels at each individual sequence. By default it
will plot the `seq_id` as label, but users are able to change this
manually.

Position of the label/text can be adjusted with the different arguments
(e.g. `vjust`, `hjust`, `angle`, etc.)

## Usage

``` r
geom_seq_label(
  mapping = NULL,
  data = seqs(),
  hjust = 0,
  vjust = 1,
  nudge_y = -0.15,
  size = 2.5,
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

- hjust:

  Moves the text horizontally

- vjust:

  Moves the text vertically

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

## Value

Sequence labels are added as a text layer/component to the plot.

## Details

This labeling function uses
[`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
under the hood. Any changes to the aesthetics of the text can be
performed in a ggplot2 manner.

## Examples

``` r
# example data
seqs <- tibble::tibble(
  bin_id = c("A", "A", "A", "B", "B", "B", "B", "C", "C"),
  seq_id = c("A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2"),
  start = c(0, 100, 200, 0, 50, 150, 250, 0, 400),
  end = c(100, 200, 400, 50, 100, 250, 300, 300, 500),
  length = c(100, 100, 200, 50, 50, 100, 50, 300, 100)
)

# example plot using geom_seq_label
gggenomes(seqs = seqs) +
  geom_seq() +
  geom_seq_label()


# changing default label to `length` column
gggenomes(seqs = seqs) +
  geom_seq() +
  geom_seq_label(aes(label = length))


# with horizontal adjustment
gggenomes(seqs = seqs) +
  geom_seq() +
  geom_seq_label(hjust = -5)


# with wrapping at 300
gggenomes(seqs = seqs, wrap = 300) +
  geom_seq() +
  geom_seq_label()
```
