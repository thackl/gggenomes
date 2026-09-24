# Layout genomes

Layout genomes

## Usage

``` r
layout_genomes(
  seqs = NULL,
  genes = NULL,
  feats = NULL,
  links = NULL,
  infer_bin_id = seq_id,
  infer_start = min(start, end),
  infer_end = max(start, end),
  infer_length = max(start, end),
  adjacent_only = TRUE,
  marginal = "drop",
  ...
)
```

## Arguments

- seqs:

  A data.frame or a character vector with paths to files containing
  sequence data. Data columns:

  - required: `seq_id,length`

  - recognized: `bin_id,start,end,strand`

- genes, feats:

  A data.frame, a list of data.frames, or a character vector with paths
  to files containing gene data. Each item is added as feature track.

  For a single data.frame the track_id will be "genes" and "feats",
  respectively. For a list, track_ids are parsed from the list names, or
  if names are missing from the name of the variable containing each
  data.frame. Data columns:

  - required: `seq_id,start,end`

  - recognized: `strand,bin_id,feat_id,introns`

- links:

  A data.frame or a character vector with paths to files containing link
  data. Each item is added as links track. Data columns:

  - required: `seq_id,seq_id2`

  - recognized: `start,end,bin_id,start2,end2,bin_id2,strand`

- infer_length, infer_start, infer_end, infer_bin_id:

  used to infer pseudo seqs if only feats or links are provided, or if
  no bin_id column was provided. The expressions are evaluated in the
  context of the first feat or link track.

  By default subregions of sequences from the first to the last
  feat/link are generated. Set `infer_start` to 0 to show all sequences
  from their true beginning.

- adjacent_only:

  Indicates whether links should be created between adjacent
  sequences/chromosomes only. By default it is set to `TRUE`. If set to
  `FALSE`, links will be created between all sequences. This is *not
  recommended for large data sets* as it slow and plots become way to
  cluttered to be legible.

- marginal:

  How to handle feats/genes and links overlapping edges of sequence
  regions when providing sequence start/end or after zooming in with
  [`focus()`](https://thackl.github.io/gggenomes/reference/focus.md).
  Choices are to "drop", "keep" or "trim", with "drop" as the default.
  You can provide two values to specify different behavior for
  feats/genes and links. See
  [`vignette("marginal", package = "gggenomes")`](https://thackl.github.io/gggenomes/articles/marginal.md)
  for more details.

- ...:

  additional parameters, passed to layout

## Value

gggenomes_layout object
