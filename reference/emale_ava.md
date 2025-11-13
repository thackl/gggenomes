# All-versus-all whole genome alignments of 6 EMALE genomes

One row per alignment block. Alignments were computed with minimap2.

## Usage

``` r
emale_ava
```

## Format

A data frame with 125 rows and 23 columns

- file_id:

  name of the file the data was read from

- seq_id:

  identifier of the sequence the feature appears on

- length:

  length of the sequence

- start:

  start of the feature on the sequence

- end:

  end of the feature on the sequence

- strand:

  orientation of the feature relative to the sequence (+ or -)

- seq_id2:

  identifier of the sequence the feature appears on

- length2:

  length of the sequence

- start2:

  start of the feature on the sequence

- end2:

  end of the feature on the sequence

- map_match, map_length, map_quality, NM, ms, AS, nn, tp, cm, s1, de,
  rl, cg:

  see <https://github.com/lh3/miniasm/blob/master/PAF.md> for additional
  columns

## Source

- Derived & bundled data: `ex("emales/emales.paf")`
