# Layout sequences

Layout sequences

## Usage

``` r
layout_seqs(
  x,
  spacing = 0.05,
  wrap = NULL,
  spacing_style = c("regular", "center", "spread"),
  keep = "strand"
)
```

## Arguments

- x:

  seq_layout

- spacing:

  between sequences in bases (\>1) or relative to longest bin (\<1)

- wrap:

  wrap bins into multiple lines with at most this many nucleotides per
  line.

- spacing_style:

  one of "regular", "center", "spread"

- keep:

  keys to keep (default: "strand")

## Value

a tbl_df with plot coordinates
