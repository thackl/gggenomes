# Read a VCF file

VCF (Variant Call Format) file format is used to store variation data
and its metadata. Based on the used analysis program (e.g. GATK,
freebayes, etc...), details within the VCF file can slightly differ. For
example, type of mutation is not mentioned as output for certain variant
analysis programs. the "read_vcf" function, ignores the first
header/metadata lines and directly converts the data into a tidy
dataframe. The function will extract the type of mutation. By absence,
it will derive the type of mutation from the "ref" and "alt" column.

## Usage

``` r
read_vcf(
  file,
  parse_info = FALSE,
  col_names = def_names("vcf"),
  col_types = def_types("vcf")
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

- parse_info:

  if set to 'TRUE', the read_vcf function will split all the metadata
  stored in the "info" column and stores it into separate columns. By
  default it is set to 'FALSE'.

- col_names:

  column names to use. Defaults to `def_names("vcf")` (see
  [`def_names`](https://thackl.github.io/gggenomes/reference/def_names.md)).

- col_types:

  column types to use. Defaults to `def_types("vcf")` (see
  [`def_types`](https://thackl.github.io/gggenomes/reference/def_names.md)).

## Value

dataframe
