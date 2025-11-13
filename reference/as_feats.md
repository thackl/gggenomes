# Compute a layout for feat data

Read feat data such as genes into a tidy dataframe and augment it with
layout information based on a sequence layout.

## Usage

``` r
as_feats(x, seqs, ..., everything = TRUE)
```

## Arguments

- x:

  feat data convertible to a feat layout

- seqs:

  the sequence layout the feat map onto.

- ...:

  passed on to
  [`layout_seqs()`](https://thackl.github.io/gggenomes/reference/layout_seqs.md)

- everything:

  set to FALSE to drop optional columns

## Value

a tbl_df with plot coordinates

## Details

Obligatory columns are `seq_id`, `start` and `end`. Also recognized are
`strand` and `bin_id`.

Note `start` and `end` for every record will be coerced so that
`start < end`. If no `strand` was provided, `strand` will added and set
to "+" for records that initially had `start < end` and "-" for
`end < start` inputs. If `strand` was provided, `start` and `end` will
be ordered without any additional effect.
