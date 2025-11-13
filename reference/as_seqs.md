# Compute a layout for sequence data

Read sequence data of multiple genomes (contigs, chromosomes, ...) into
a tidy dataframe and augment it with layout information necessary for
plotting.

## Usage

``` r
as_seqs(x, ...)

# S3 method for class 'tbl_df'
as_seqs(x, everything = TRUE, ...)
```

## Arguments

- x:

  an object convertible to a sequence layout

- ...:

  pass through to
  [`layout_seqs()`](https://thackl.github.io/gggenomes/reference/layout_seqs.md)

- everything:

  set to FALSE to drop optional columns

## Value

an tbl_df with plot coordinates

## Details

Obligatory columns are `seq_id`, `bin_id` and `length`.

## Methods (by class)

- `as_seqs(tbl_df)`: Convert a list of tibbles into a seq layout

## Examples

``` r
chr <- tibble::tibble(
  seq_id = c("a1", "b1", "b2"),
  bin_id = c(rep("A", 1), rep("B", 2)),
  length = c(5000, 3000, 1400)
)

as_seqs(chr)
```
