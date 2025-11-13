# Read files in different contexts

Powers
[`read_seqs()`](https://thackl.github.io/gggenomes/reference/read_tracks.md),
[`read_feats()`](https://thackl.github.io/gggenomes/reference/read_tracks.md),
[`read_links()`](https://thackl.github.io/gggenomes/reference/read_tracks.md)

## Usage

``` r
read_context(
  files,
  context,
  .id = "file_id",
  format = NULL,
  parser = NULL,
  ...
)
```

## Arguments

- files:

  files to reads. Should all be of same format. In many cases,
  compressed files (`.gz`, `.bz2`, `.xz`, or `.zip`) are supported.
  Similarly, automatic download of remote files starting with
  `http(s)://` or `ftp(s)://` works in most cases.

- context:

  the context ("seqs", "feats", "links") in which a given format should
  be read.

- .id:

  the column with the name of the file a record was read from. Defaults
  to "file_id". Set to "bin_id" if every file represents a different
  bin.

- format:

  specify a format known to gggenomes, such as `gff3`, `gbk`, ... to
  overwrite automatic determination based on the file extension (see
  [`def_formats()`](https://thackl.github.io/gggenomes/reference/def_formats.md)
  for full list).

- parser:

  specify the name of an R function to overwrite automatic determination
  based on format, e.g. `parser="read_tsv"`.

- ...:

  additional arguments passed on to the format-specific read function
  called down the line.

## Value

a tibble with the combined data from all files

## Functions

- `read_context()`: bla keywords internal
