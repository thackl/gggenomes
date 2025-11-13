# Compute a layout for link data

Read link data of pairwise sequence or feat comparisons, such as
similarity searches into a tidy dataframe and augment it with layout
information based on a sequence layout.

## Usage

``` r
as_links(x, seqs, ..., everything = TRUE)
```

## Arguments

- x:

  link data convertible to a link layout

- seqs:

  the sequence layout the feat map onto.

- ...:

  passed on to
  [`layout_seqs()`](https://thackl.github.io/gggenomes/reference/layout_seqs.md)

- everything:

  set to FALSE to drop optional columns

## Value

a link layout

## Details

Obligatory columns are `seq_id` and `seq_id2`. Also recognized are
`start`, `end`,`start2`,`end2`,`strand`, bin_id`and`bin_id2.

During layouting, seq_id,start,end will be projected to x,xend,y, while
seq_id2,start2,end2 will be projected to xmin,xmax,yend. gggenomes uses
these maybe a bit odd names for the variables here, is so that they play
nice with ggplots native transformation functions for position
aesthetics. Those only work well with a specific set of predefined var
names, which include those used above.
