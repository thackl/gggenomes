# Write a gff3 file from a tidy table

Write a gff3 file from a tidy table

## Usage

``` r
write_gff3(
  feats,
  file,
  seqs = NULL,
  type = NULL,
  source = ".",
  score = ".",
  strand = ".",
  phase = ".",
  id_var = "feat_id",
  parent_var = "parent_ids",
  head = "##gff-version 3",
  ignore_attr = c("introns", "geom_id")
)
```

## Arguments

- feats:

  tidy feat table

- file:

  name of output file

- seqs:

  a tidy sequence table to generate optional `##sequence-region`
  directives in the header

- type:

  if no type column exists, use this as the default type

- source:

  if no source column exists, use this as the default source

- score:

  if no score column exists, use this as the default score

- strand:

  if no strand column exists, use this as the default strand

- phase:

  if no phase column exists, use this as the default phase

- id_var:

  the name of the column to use as the GFF3 `ID` tag

- parent_var:

  the name of the column to use as GFF3 `Parent` tag

- head:

  additional information to add to the header section

- ignore_attr:

  attributes not to be included in GFF3 tag list. Defaults to internals:
  `introns, geom_id`

## Value

No return value, writes to file

## Examples

``` r
filename <- tempfile(fileext = ".gff")
write_gff3(emale_genes, filename, emale_seqs, id_var = "feat_id")
```
