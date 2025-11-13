# Introduce non-existing columns

Works like
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
but without changing existing columns, but only adding new ones. Useful
to add possibly missing columns with default values.

## Usage

``` r
introduce(.data, ...)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs. The name gives the name of the column in the output.

  The value can be:

  - A vector of length 1, which will be recycled to the correct length.

  - A vector the same length as the current group (or the whole data
    frame if ungrouped).

  - `NULL`, to remove the column.

  - A data frame or tibble, to create multiple columns in the output.

## Value

a tibble with new columns

## Examples

``` r
# ensure columns "y" and "z" exist
tibble::tibble(x = 1:3) %>%
  introduce(y = "a", z = paste0(y, dplyr::row_number()))
#> # A tibble: 3 × 3
#>       x y     z    
#>   <int> <chr> <chr>
#> 1     1 a     a1   
#> 2     2 a     a2   
#> 3     3 a     a3   
# ensure columns "y" and "z" exist, but do not overwrite "y"
tibble::tibble(x = 1:3, y = c("c", "d", "e")) %>%
  introduce(y = "a", z = paste0(y, dplyr::row_number()))
#> # A tibble: 3 × 3
#>       x y     z    
#>   <int> <chr> <chr>
#> 1     1 c     c1   
#> 2     2 d     d2   
#> 3     3 e     e3   
```
