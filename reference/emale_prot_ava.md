# All-versus-all alignments 6 EMALE proteomes

One row per alignment. Alignments were computed with mmseqs2
(blast-like).

## Usage

``` r
emale_prot_ava
```

## Format

A data frame with 827 rows and 13 columns

- file_id:

  name of the file the data was read from

- feat_id:

  identifier of the first feature in the alignment

- feat_id2:

  identifier of the second feature in the alignment

- pident, length, mismatch, gapopen, start, end, start2, end2, evalue,
  bitscore:

  see <https://github.com/seqan/lambda/wiki/BLAST-Output-Formats> for
  BLAST-tabular format columns

## Source

- Derived & bundled data: `ex("emales/emales-prot-ava.o6")`
