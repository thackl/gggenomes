# The width of a range

Always returns a positive value, even if start \> end. `width0` is a
short handle for `width(..., base=0)`

## Usage

``` r
width(start, end, base = 1)

width0(start, end, base = 0)
```

## Arguments

- start, end:

  start and end of the range

- base:

  the base of the coordinate system, usually 1 or 0.

## Value

a numeric vector
