# Relative GC-content along 6 EMALE genomes

One row per 50 bp window.

## Usage

``` r
emale_gc
```

## Format

A data frame with 2856 rows and 6 columns

- file_id:

  name of the file the data was read from

- seq_id:

  identifier of the sequence the feature appears on

- start:

  start of the feature on the sequence

- end:

  end of the feature on the sequence

- name:

  name of the feature

- score:

  relative GC-content of the window

## Source

- Derived & bundled data: `ex("emales/emales-gc.bed")`
