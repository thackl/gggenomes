# Defined file formats and extensions

For seamless reading of different file formats, gggenomes uses a mapping
of known formats to associated file extensions and contexts in which the
different formats can be read. The notion of context allows one to read
different information from the same format/extension. For example, a gbk
file holds both feature and sequence information. If read in "feats"
context `read_feats("*.gbk")` it will return a feature table, if read in
"seqs" context `read_seqs("*.gbk")`, a sequence index.

## Usage

``` r
def_formats(
  file = NULL,
  ext = NULL,
  context = NULL,
  parser = NULL,
  allow_na = FALSE
)
```

## Arguments

- file:

  a vector of file names

- ext:

  a vector of file extensions

- context:

  a vector of file contexts defined in `gggenomes_global$def_formats`

- parser:

  a vector of file parsers defined in `gggenomes_global$def_formats`

- allow_na:

  boolean

## Value

dictionarish vector of file formats with recognized extensions as names

## Defined formats, extensions, contexts, and parsers

          format                           ext            context                                              parser
    1  ambigious                 txt, tsv, csv                 NA                                      read_ambigious
    2      fasta fa, fas, fasta, ffn, fna, faa               seqs                                        read_seq_len
    3        fai                           fai               seqs                                            read_fai
    4       gff3          gff, gff3, gff2, gtf        feats, seqs                             read_gff3, read_seq_len
    5        gbk           gbk, gb, gbff, gpff        feats, seqs                              read_gbk, read_seq_len
    6        bed                           bed              feats                                            read_bed
    7      blast                    m8, o6, o7       feats, links                              read_blast, read_blast
    8        paf                           paf       feats, links                                  read_paf, read_paf
    9      alitv                          json feats, seqs, links read_alitv_genes, read_alitv_seqs, read_alitv_links
    10       vcf                           vcf              feats                                            read_vcf

## Examples

``` r
# vector of defined zip formats and recognized extensions as names
# format of file
def_formats("foo.fa")
#>      fa 
#> "fasta" 

# formats associated with each extension
def_formats(ext = c("fa", "gff"))
#>      fa     gff 
#> "fasta"  "gff3" 

# all formats/extensions that can be read in seqs context; includes formats
# that are defined for context=NA, i.e. that can be read in any context.
def_formats(context = "seqs")
#>        json         txt         tsv         csv         fai          fa 
#>     "alitv" "ambigious" "ambigious" "ambigious"       "fai"     "fasta" 
#>         fas       fasta         ffn         fna         faa         gbk 
#>     "fasta"     "fasta"     "fasta"     "fasta"     "fasta"       "gbk" 
#>          gb        gbff        gpff         gff        gff3        gff2 
#>       "gbk"       "gbk"       "gbk"      "gff3"      "gff3"      "gff3" 
#>         gtf 
#>      "gff3" 
```
