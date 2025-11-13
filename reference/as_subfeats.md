# Compute a layout for subfeat data

Read subfeat data such as domains or blast hits on genes into a tidy
dataframe. Subfeats need to be associated with an already added feat
track. The subfeat track itself is internally converted into a new,
regular feat track by mapping the `start` and `end` coordinates provided
relative to their parent feat into coordinates relative to the sequences
underlying the parent feats.

## Usage

``` r
as_subfeats(x, seqs, feats, ..., everything = TRUE)

# S3 method for class 'tbl_df'
as_subfeats(
  x,
  seqs,
  feats,
  ...,
  everything = TRUE,
  transform = c("none", "aa2nuc", "nuc2aa")
)
```

## Arguments

- x:

  subfeat data convertible to a feat layout

- seqs:

  the sequence layout the parent feats map onto.

- feats:

  the parent feats the subfeats map onto.

- ...:

  passed on to
  [`layout_seqs()`](https://thackl.github.io/gggenomes/reference/layout_seqs.md)
  spaces, i.e. if matching nucleotide-level annotations to protein level
  annotations, e.g. genes and protein blast results.

- everything:

  set to FALSE to drop optional columns

- transform:

  use if feats and subfeats are in different coordinate

## Value

a tbl_df with plot coordinates

## Details

Obligatory columns are `feat_id`, `start` and `end`. Also recognized are
`strand` and `bin_id`.

Note `start` and `end` for every record will be coerced so that
`start < end`. If no `strand` was provided, `strand` will be added and
set to "+" for records that initially had `start < end` and "-" for
`end < start` inputs. If `strand` was provided, `start` and `end` will
be reorganized to conform with `start < end` without any additional
effect.

## Methods (by class)

- `as_subfeats(tbl_df)`: Convert a list of tibbles into a feat layout
