# `ggplot2::facet_null` checks data with `empty(df)` using `dim`. This causes an error because dim(gggenome_layout) is undefined. Return dim of primary table instead

[`ggplot2::facet_null`](https://ggplot2.tidyverse.org/reference/facet_null.html)
checks data with `empty(df)` using `dim`. This causes an error because
dim(gggenome_layout) is undefined. Return dim of primary table instead

## Usage

``` r
# S3 method for class 'gggenomes_layout'
dim(x)
```

## Value

dim of primary table
