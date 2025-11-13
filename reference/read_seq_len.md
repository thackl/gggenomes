# Read sequence index

Read sequence index

## Usage

``` r
read_seq_len(file)

read_fai(file, col_names = def_names("fai"), col_types = def_types("fai"), ...)
```

## Arguments

- file:

  with sequence length information

- col_names:

  Either `TRUE`, `FALSE` or a character vector of column names.

  If `TRUE`, the first row of the input will be used as the column
  names, and will not be included in the data frame. If `FALSE`, column
  names will be generated automatically: X1, X2, X3 etc.

  If `col_names` is a character vector, the values will be used as the
  names of the columns, and the first row of the input will be read into
  the first row of the output data frame.

  Missing (`NA`) column names will generate a warning, and be filled in
  with dummy names `...1`, `...2` etc. Duplicate column names will
  generate a warning and be made unique, see `name_repair` to control
  how this is done.

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

tibble with sequence information

tibble with sequence information

## Functions

- `read_seq_len()`: read seqs from a single file_name in fasta, gbk or
  gff3 format.

- `read_fai()`: read seqs from a single file in seqkit/samtools fai
  format.
