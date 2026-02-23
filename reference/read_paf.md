# Read a .paf file (minimap/minimap2).

Read a minimap/minimap2 .paf file including optional tagged extra
fields. The optional fields will be parsed into a tidy format, one
column per tag.

## Usage

``` r
read_paf(
  file,
  max_tags = 20,
  col_names = def_names("paf"),
  col_types = def_types("paf"),
  ...
)
```

## Arguments

- file:

  Either a path to a file, a connection, or literal data (either a
  single string or a raw vector). `file` can also be a character vector
  containing multiple filepaths or a list containing multiple
  connections.

  Files ending in `.gz`, `.bz2`, `.xz`, or `.zip` will be automatically
  decompressed. Files starting with `http://`, `https://`, `ftp://`, or
  `ftps://` will be automatically downloaded. Remote compressed files
  (`.gz`, `.bz2`, `.xz`, `.zip`) will be automatically downloaded and
  decompressed.

  Literal data is most useful for examples and tests. To be recognised
  as literal data, wrap the input with
  [`I()`](https://rdrr.io/r/base/AsIs.html).

- max_tags:

  maximum number of optional fields to include

- col_names:

  column names to use. Defaults to `def_names("gff3")` (see
  [`def_names`](https://thackl.github.io/gggenomes/reference/def_names.md)).

- col_types:

  column types to use. Defaults to `def_types("gff3")` (see
  [`def_types`](https://thackl.github.io/gggenomes/reference/def_names.md)).

- ...:

  additional parameters, passed to `read_tsv`

## Value

tibble

## Details

Because
[`readr::read_tsv`](https://readr.tidyverse.org/reference/read_delim.html)
expects a fixed number of columns, but in .paf the number of optional
fields can differ among records, `read_paf` tries to read at least as
many columns as the longest record has (`max_tags`). The resulting
warnings for each record with fewer fields of the form "32 columns
expected, only 22 seen" should thus be ignored.

From the minimap2 manual

+—-+——–+———————————————————+ \|Col \| Type \| Description \|
+—-+——–+———————————————————+ \| 1 \| string \| Query sequence name \| \|
2 \| int \| Query sequence length \| \| 3 \| int \| Query start
coordinate (0-based) \| \| 4 \| int \| Query end coordinate (0-based) \|
\| 5 \| char \| ‘+’ if query/target on the same strand; ‘-’ if opposite
\| \| 6 \| string \| Target sequence name \| \| 7 \| int \| Target
sequence length \| \| 8 \| int \| Target start coordinate on the
original strand \| \| 9 \| int \| Target end coordinate on the original
strand \| \| 10 \| int \| Number of matching bases in the mapping \| \|
11 \| int \| Number bases, including gaps, in the mapping \| \| 12 \|
int \| Mapping quality (0-255 with 255 for missing) \|
+—-+——–+———————————————————+

+—-+——+——————————————————-+ \|Tag \| Type \| Description \|
+—-+——+——————————————————-+ \| tp \| A \| Type of aln: P/primary,
S/secondary and I,i/inversion \| \| cm \| i \| Number of minimizers on
the chain \| \| s1 \| i \| Chaining score \| \| s2 \| i \| Chaining
score of the best secondary chain \| \| NM \| i \| Total number of
mismatches and gaps in the alignment \| \| MD \| Z \| To generate the
ref sequence in the alignment \| \| AS \| i \| DP alignment score \| \|
ms \| i \| DP score of the max scoring segment in the alignment \| \| nn
\| i \| Number of ambiguous bases in the alignment \| \| ts \| A \|
Transcript strand (splice mode only) \| \| cg \| Z \| CIGAR string (only
in PAF) \| \| cs \| Z \| Difference string \| \| dv \| f \| Approximate
per-base sequence divergence \| +—-+——+——————————————————-+

From https://samtools.github.io/hts-specs/SAMtags.pdf type may be one of
A (character), B (general array), f (real number), H (hexadecimal
array), i (integer), or Z (string).
