# Read a BED file

BED files use 0-based coordinate starts, while gggenomes uses 1-based
start coordinates. BED file coordinates are therefore transformed into
1-based coordinates during import.

## Usage

``` r
read_bed(file, col_names = def_names("bed"), col_types = def_types("bed"), ...)
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

  column names to use. Defaults to `def_names("bed")` compatible with
  canonical bed files.
  [`def_names()`](https://thackl.github.io/gggenomes/reference/def_names.md)
  can easily be combined with extra columns:
  `col_names = c(def_names("bed"), "more", "things")`.

- col_types:

  One of `NULL`, a
  [`cols()`](https://readr.tidyverse.org/reference/cols.html)
  specification, or a string. See `vignette("readr")` for more details.

  If `NULL`, all column types will be inferred from `guess_max` rows of
  the input, interspersed throughout the file. This is convenient (and
  fast), but not robust. If the guessed types are wrong, you'll need to
  increase `guess_max` or supply the correct types yourself.

  Column specifications created by
  [`list()`](https://rdrr.io/r/base/list.html) or
  [`cols()`](https://readr.tidyverse.org/reference/cols.html) must
  contain one column specification for each column. If you only want to
  read a subset of the columns, use
  [`cols_only()`](https://readr.tidyverse.org/reference/cols.html).

  Alternatively, you can use a compact string representation where each
  character represents one column:

  - c = character

  - i = integer

  - n = number

  - d = double

  - l = logical

  - f = factor

  - D = date

  - T = date time

  - t = time

  - ? = guess

  - \_ or - = skip

  By default, reading a file without a column specification will print a
  message showing what `readr` guessed they were. To remove this
  message, set `show_col_types = FALSE` or set
  `options(readr.show_col_types = FALSE)`.

- ...:

  additional parameters, passed to `read_tsv`

## Value

tibble
