# Resolve relative or absolute scalebar length

`0 < length < 1` is interpreted as a target fraction of `span`, rounded
down to a nice value. `length >= 1` is interpreted as absolute x units.

## Usage

``` r
resolve_scalebar_length(length = 0.25, span)
```
