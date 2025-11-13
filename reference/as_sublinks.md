# Compute a layout for links linking feats

Reads sublinks connecting feats such as all-vs-all protein blasts into a
tidy dataframe. sublinks need to be associated with an already added
feat track. The sublinks are internally converted into a regular link
track by mapping the feat-based `start` and `end` coordinates to
coordinates relative to the sequences underlying the linked feats.

## Usage

``` r
as_sublinks(x, seqs, feats, ..., everything = TRUE)

# S3 method for class 'tbl_df'
as_sublinks(
  x,
  seqs,
  feats,
  ...,
  everything = TRUE,
  transform = c("none", "aa2nuc", "nuc2aa"),
  compute_layout = TRUE
)
```

## Arguments

- x:

  sublink data convertible to a link layout

- seqs:

  the sequence layout the linked feats map onto.

- feats:

  the feats the sublinks map onto.

- ...:

  passed on to
  [`layout_seqs()`](https://thackl.github.io/gggenomes/reference/layout_seqs.md)
  spaces, i.e. if matching nucleotide-level annotations to protein level
  annotations, e.g. genes and protein blast results.

- everything:

  set to FALSE to drop optional columns

- transform:

  use if feats and sublinks are in different coordinate

- compute_layout:

  set to FALSE to skip layout computation

## Value

a tbl_df with plot coordinates

## Details

The only obligatory columns are `feat_id` & `feat_id2`. Also recognized
are `start/end`, `start2/end2` and `strand`.

Note `start` and `end` for every record will be coerced so that
`start < end`. If no `strand` was provided, `strand` will be added and
set to "+" for records that initially had `start < end == start2 < end2`
and "-" otherwise. If `strand` was provided, `start` and `end` will be
reorganized to conform with `start < end` without any additional effect.

## Methods (by class)

- `as_sublinks(tbl_df)`: Convert a list of tibbles into a link layout
