# Default column names and types for defined formats

Intended to be used in
[`readr::read_tsv()`](https://readr.tidyverse.org/reference/read_delim.html)-like
functions that accept a `col_names` and a `col_types` argument.

## Usage

``` r
def_names(format)

def_types(format)
```

## Arguments

- format:

  specify a format known to gggenomes, such as `gff3`, `gbk`, ...

## Value

a vector with default column names for the given format

a vector with default column types for the given format

## Functions

- `def_names()`: default column names for defined formats

- `def_types()`: default column types for defined formats

## Defined formats, column types and names

      gff3       ccciicccc       seq_id,source,type,start,end,score,strand,phase,attributes
      paf        ciiicciiiiid    seq_id,length,start,end,strand,seq_id2,length2,start2,end2,map_match,map_length,map_quality
      blast      ccdiiiiiiidd    seq_id,seq_id2,pident,length,mismatch,gapopen,start,end,start2,end2,evalue,bitscore
      bed        ciicdc          seq_id,start,end,name,score,strand
      fai        ci---           seq_id,seq_desc,length
      seq_len    cci             seq_id,seq_desc,length
      vcf        cicccdccc       seq_id,start,feat_id,ref,alt,qual,filter,info,format

## Examples

``` r
# read a blast-tabular file with read_tsv
readr::read_tsv(ex("emales/emales-prot-ava.o6"), col_names = def_names("blast"))
#> Rows: 827 Columns: 12
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr  (2): seq_id, seq_id2
#> dbl (10): pident, length, mismatch, gapopen, start, end, start2, end2, evalu...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> # A tibble: 827 × 12
#>    seq_id        seq_id2 pident length mismatch gapopen start   end start2  end2
#>    <chr>         <chr>    <dbl>  <dbl>    <dbl>   <dbl> <dbl> <dbl>  <dbl> <dbl>
#>  1 Cflag_017B_0… Cflag_…  1        119        0       0     1   119      1   119
#>  2 Cflag_017B_0… RCC970…  1        119        0       0     1   119      1   119
#>  3 Cflag_017B_0… E4-10_…  0.991    119        1       0     1   119      1   119
#>  4 Cflag_017B_0… BVI_06…  0.991    119        1       0     1   119      1   119
#>  5 Cflag_017B_0… E4-10_…  0.983    119        2       0     1   119      1   119
#>  6 Cflag_017B_0… BVI_00…  0.983    119        2       0     1   119      1   119
#>  7 E4-10_112_00… E4-10_…  1        406        0       0     1   406      1   406
#>  8 E4-10_112_00… E4-10_…  1        406        0       0     1   406      1   406
#>  9 E4-10_112_00… BVI_06…  0.997    406        1       0     1   406      1   406
#> 10 E4-10_112_00… BVI_00…  0.987    406        5       0     1   406      1   406
#> # ℹ 817 more rows
#> # ℹ 2 more variables: evalue <dbl>, bitscore <dbl>
```
