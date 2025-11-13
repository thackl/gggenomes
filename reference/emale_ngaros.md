# Integrated Ngaro retrotransposons of 6 EMALE genomes

Integrated Ngaro retrotransposons of 6 EMALE genomes

## Usage

``` r
emale_ngaros
```

## Format

A data frame with 3 rows and 14 columns

- file_id:

  name of the file the data was read from

- seq_id:

  identifier of the sequence the feature appears on

- start:

  start of the feature on the sequence

- end:

  end of the feature on the sequence

- strand:

  orientation of the feature relative to the sequence (+ or -)

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

- name:

  name of the feature

- geom_id:

  an identifier telling the which features should be plotted as on items
  (usually CDS and mRNA of same gene)

## Source

- Publication:
  [doi:10.1101/2020.11.30.404863](https://doi.org/10.1101/2020.11.30.404863)

- Raw data: <https://github.com/thackl/cb-emales>

- Derived & bundled data: `ex("emales/emales-ngaros.gff")`
