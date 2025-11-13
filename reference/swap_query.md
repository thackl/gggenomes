# Swap query and subject in blast-like feature tables

Swap query and subject columns in a table read with
[`read_feats()`](https://thackl.github.io/gggenomes/reference/read_tracks.md)
or
[`read_links()`](https://thackl.github.io/gggenomes/reference/read_tracks.md),
for example, from blast searches. Swaps columns with name/name2, such as
'seq_id/seq_id2', 'start/start2', ...

## Usage

``` r
swap_query(x)
```

## Arguments

- x:

  tibble with query and subject columns

## Value

tibble with swapped query/subject columns

## Examples

``` r
feats <- tibble::tribble(
  ~seq_id, ~seq_id2, ~start, ~end, ~strand, ~start2, ~end2, ~evalue,
  "A", "B", 100, 200, "+", 10000, 10200, 1e-5
)
# make B the query
swap_query(feats)
#> Swapping query/subject-associated columns
#> • seq_id  start  end
#> • seq_id2 start2 end2
#> # A tibble: 1 × 8
#>   seq_id seq_id2 start   end strand start2  end2  evalue
#>   <chr>  <chr>   <dbl> <dbl> <chr>   <dbl> <dbl>   <dbl>
#> 1 B      A       10000 10200 +         100   200 0.00001
```
