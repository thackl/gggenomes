# Basic info on tracks in a gggenomes object

Use `track_info()` to call on a gggenomes or gggenomes_layout object to
return a short tibble with ids, types, index and size of the loaded
tracks.

## Usage

``` r
track_info(x, ...)
```

## Arguments

- x:

  A gggenomes or gggenomes_layout object

- ...:

  unused

## Value

Short tibble with ids, types, index and size of loaded tracks.

## Details

The short tibble contains basic information on the tracks within the
entered gggenomes object.

- **id** : Shows original name of inputted data frame (only when more
  than one data frames are present in a track).

- **type** : The track in which the data frame is present.

- **i** (index) : The chronological order of data frames in a specific
  track.

- **n** (size) : Amount of objects **plotted** from the data frame.
  (**not** the amount of objects *in* the inputted data frame)

## Examples

``` r
gggenomes(
  seqs = emale_seqs,
  feats = list(emale_genes, emale_tirs, emale_ngaros),
  links = emale_ava
) |>
  track_info()
#> # A tibble: 5 × 4
#> # Groups:   type [3]
#>   id           type      i     n
#>   <chr>        <chr> <int> <dbl>
#> 1 seqs         seqs      1     6
#> 2 emale_genes  feats     1   143
#> 3 emale_tirs   feats     2    12
#> 4 emale_ngaros feats     3     3
#> 5 links        links     1    38
```
