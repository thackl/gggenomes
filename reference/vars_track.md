# Tidyselect track variables

Based on
[`tidyselect::vars_pull`](https://tidyselect.r-lib.org/reference/vars_pull.html).
Powers track selection in
[`pull_track()`](https://thackl.github.io/gggenomes/reference/pull_track.md).
Catches and modifies errors from vars_pull to track-relevant info.

## Usage

``` r
vars_track(
  x,
  track_id,
  track_type = c("seqs", "feats", "links"),
  ignore = NULL
)
```

## Arguments

- x:

  A gggenomes or gggenomes_layout object

- track_id:

  a quoted or unquoted name or as positive/negative integer giving the
  position from the left/right.

- track_type:

  restrict to these types of tracks - affects position-based selection

- ignore:

  names of tracks to ignore when selecting by position.

## Value

The selected track_id as an unnamed string
