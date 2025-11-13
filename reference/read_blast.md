# Read BLAST tab-separated output

Read BLAST tab-separated output

## Usage

``` r
read_blast(
  file,
  col_names = def_names("blast"),
  col_types = def_types("blast"),
  comment = "#",
  swap_query = FALSE,
  ...
)
```

## Arguments

- file:

  Either a path to a file, a connection, or literal data (either a
  single string or a raw vector).

  Files ending in `.gz`, `.bz2`, `.xz`, or `.zip` will be automatically
  uncompressed. Files starting with `http://`, `https://`, `ftp://`, or
  `ftps://` will be automatically downloaded. Remote gz files can also
  be automatically downloaded and decompressed.

  Literal data is most useful for examples and tests. To be recognised
  as literal data, the input must be either wrapped with
  [`I()`](https://rdrr.io/r/base/AsIs.html), be a string containing at
  least one new line, or be a vector containing at least one string with
  a new line.

  Using a value of
  [`clipboard()`](https://readr.tidyverse.org/reference/clipboard.html)
  will read from the system clipboard.

- col_names:

  column names to use. Defaults to `def_names("blast")` compatible with
  blast tabular output (`--outfmt 6/7` in blast++ and `-m8` in
  blast-legacy).
  [`def_names()`](https://thackl.github.io/gggenomes/reference/def_names.md)
  can easily be combined with extra columns:
  `col_names = c(def_names("blast"), "more", "things")`.

- col_types:

  column types to use. Defaults to `def_types("gff3")` (see
  [`def_types`](https://thackl.github.io/gggenomes/reference/def_names.md)).

- comment:

  character

- swap_query:

  if TRUE swap query and subject columns using
  [`swap_query()`](https://thackl.github.io/gggenomes/reference/swap_query.md)
  on import.

- ...:

  additional parameters, passed to `read_tsv`

## Value

a tibble with the BLAST output
