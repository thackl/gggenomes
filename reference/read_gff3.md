# Read features from GFF3 (and with some limitations GFF2/GTF) files

Files with `##FASTA` section work but result in parsing problems for all
lines of the fasta section. Just ignore those warnings, or strip the
fasta section ahead of time from the file.

## Usage

``` r
read_gff3(
  file,
  sources = NULL,
  types = NULL,
  infer_cds_parents = is_gff2,
  sort_exons = TRUE,
  col_names = def_names("gff3"),
  col_types = def_types("gff3"),
  keep_attr = FALSE,
  fix_augustus_cds = TRUE,
  is_gff2 = NULL
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

- sort_exons:

  make sure that exons/introns appear sorted. Default TRUE. Set to FALSE
  to read CDS/exon order exactly as present in the file, which is less
  robust, but faster and allows non-canonical splicing
  (exon1-exon3-exon2).

- col_names:

  column names to use. Defaults to `def_names("gff3")` (see
  [`def_names`](https://thackl.github.io/gggenomes/reference/def_names.md)).

- col_types:

  column types to use. Defaults to `def_types("gff3")` (see
  [`def_types`](https://thackl.github.io/gggenomes/reference/def_names.md)).

- keep_attr:

  keep the original attributes column also after parsing tag=value pairs
  into tidy columns.

- fix_augustus_cds:

  If true, assume Augustus gff with bad CDS IDs that need fixing

- is_gff2:

  set if file is in gff2 format

## Value

tibble
