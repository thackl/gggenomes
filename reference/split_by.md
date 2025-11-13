# Split by key preserving order

Split by key column while preserving order according to the first
occurrence. R base split converts keys to factors, changing default
order to alphanumeric.

## Usage

``` r
split_by(.data, key)
```

## Arguments

- key:

  variable to split by

## Value

a list of tibbles

## Examples

``` r
tibble::tibble(x = c(1, 1, 1, 2), y = c("B", "A", "B", "B"), z = "foo") %>%
  split_by(x)
#> $`1`
#> # A tibble: 3 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     1 B     foo  
#> 2     1 A     foo  
#> 3     1 B     foo  
#> 
#> $`2`
#> # A tibble: 1 × 3
#>       x y     z    
#>   <dbl> <chr> <chr>
#> 1     2 B     foo  
#> 
```
