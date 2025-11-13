# Gene annotations if 6 EMALE genomes (endogenous virophages)

A data set containing gene feature annotations for 6 endogenous
virophages found in the genomes of the marine protist *Cafeteria
burkhardae*.

## Usage

``` r
emale_genes
```

## Format

A data frame with 143 rows and 17 columns

- file_id:

  name of the file the data was read from

- seq_id:

  identifier of the sequence the feature appears on

- start:

  start of the feature on the sequence

- end:

  end of the feature on the sequence

- strand:

  reading orientation relative to sequence (+ or -)

- type:

  feature type (CDS, mRNA, gene, ...)

- feat_id:

  unique identifier of the feature

- introns:

  a list column with internal intron start/end positions

- parent_ids:

  a list column with parent IDs - feat_id's of parent features

- source:

  source of the annotation

- score:

  score of the annotation

- phase:

  For "CDS" features indicates where the next codon begins relative to
  the 5' start

- width:

  width of the feature

- gc_content:

  relative GC-content of the feature

- name:

  name of the feature

- Note:

- geom_id:

  an identifier telling the which features should be plotted as on items
  (usually CDS and mRNA of same gene)

## Source

- Publication:
  [doi:10.1101/2020.11.30.404863](https://doi.org/10.1101/2020.11.30.404863)

- Raw data: <https://github.com/thackl/cb-emales>

- Derived & bundled data: `ex("emales/emales.gff")`
