# Read genbank files

Genbank flat files (.gb/.gbk/.gbff) and their ENA and DDBJ equivalents
have a particularly gruesome format. That's why `read_gbk()` is just a
wrapper around a Perl-based `gb2gff` converter and
[`read_gff3()`](https://thackl.github.io/gggenomes/reference/read_gff3.md).

## Usage

``` r
read_gbk(file, sources = NULL, types = NULL, infer_cds_parents = TRUE)
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

- sources:

  only return features from these sources

- types:

  only return features of these types, e.g. gene, CDS, ...

- infer_cds_parents:

  infer the mRNA parent for CDS features based on overlapping
  coordinates. Default TRUE for gff2/gtf, FALSE for gff3. In most GFFs
  this is properly set, but sometimes this information is missing.
  Generally, this is not a problem, however, geom_gene calls parse the
  parent information to determine which CDS and mRNAs are part of the
  same gene model. Without the parent info, mRNA and CDS are plotted as
  individual features.

## Value

tibble
