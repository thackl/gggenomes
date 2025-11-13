# Create a vector from unquoted words.

Similar to perl's `qw()`, however, in R spaces between args in function
call always cause an error, so `qw(foo bar)` wouldn't work. Workaround
is either a single string split at spaces, or unquoted elements,
separated by commas.

## Usage

``` r
qw(x)

qc(...)
```

## Arguments

- x:

  A single string of elements to be split at whitespace chars.

- ...:

  Unquoted words, separated by comma.

## Value

A vector of quoted words.

## Details

Took inspiration from
[stackoverflow/qw](https://stackoverflow.com/questions/520810/does-r-have-quote-like-operators-like-perls-qw)
and [github/Jarrett
Byrnes](https://github.com/jebyrnes/multifunc/blob/master/R/qw.R)

## Examples

``` r
qw("foo bar") # with a strsplit
#> [1] "foo" "bar"
qc(foo, bar) # or unquoted, but with commas
#> [1] "foo" "bar"
```
