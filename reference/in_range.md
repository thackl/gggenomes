# Do numeric values fall into specified ranges?

Do numeric values fall into specified ranges?

## Usage

``` r
in_range(x, left, right, closed = TRUE)
```

## Arguments

- x:

  a numeric vector of values

- left, right:

  boundary values or vectors of same length as x

- closed:

  whether to include (`TRUE`) or exclude (`FALSE`) the endpoints.
  Provide 2 values for different behaviors for lower and upper boundary,
  e.g. `c(TRUE, FALSE)` to include only the lower boundary.

## Value

a logical vector of the same length as the input

## Examples

``` r
in_range(1:5, 2, 4)
#> [1] FALSE  TRUE  TRUE  TRUE FALSE
in_range(1:5, 2, 4, closed = c(FALSE, TRUE)) # left-open
#> [1] FALSE FALSE  TRUE  TRUE FALSE
in_range(1:5, 6:2, 3) # vector of boundaries, single values recycle
#> [1] FALSE FALSE  TRUE FALSE FALSE


# plays nicely with dplyr
df <- tibble::tibble(x = rep(4, 5), left = 1:5, right = 3:7)
dplyr::mutate(df,
  closed = in_range(x, left, right, TRUE),
  open = in_range(x, left, right, FALSE)
)
#> # A tibble: 5 × 5
#>       x  left right closed open 
#>   <dbl> <int> <int> <lgl>  <lgl>
#> 1     4     1     3 FALSE  FALSE
#> 2     4     2     4 TRUE   FALSE
#> 3     4     3     5 TRUE   TRUE 
#> 4     4     4     6 TRUE   FALSE
#> 5     4     5     7 FALSE  FALSE
```
