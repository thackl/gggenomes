# Swap values of two columns based on a condition

Swap values of two columns based on a condition

## Usage

``` r
swap_if(x, condition, ...)
```

## Arguments

- x:

  a tibble

- condition:

  an expression to be evaluated in data context returning a TRUE/FALSE
  vector

- ...:

  the two columns between which values are to be swapped in
  dplyr::select-like syntax

## Value

a tibble with conditionally swapped start and end

## Examples

``` r
x <- tibble::tibble(start = c(10, 100), end = c(30, 50))
# ensure start of a range is always smaller than the end
swap_if(x, start > end, start, end)
#> # A tibble: 2 × 2
#>   start   end
#>   <dbl> <dbl>
#> 1    10    30
#> 2    50   100
```
