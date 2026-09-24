# Read files in various standard formats (FASTA, GFF3, GBK, BED, BLAST, ...) into track tables

Convenience functions to read sequences, features or links from various
bioinformatics file formats, such as FASTA, GFF3, Genbank, BLAST tabular
output, etc. See
[`def_formats()`](https://thackl.github.io/gggenomes/reference/def_formats.md)
for full list. File formats and the corresponding read-functions are
automatically determined based on file extensions. All these functions
can read multiple files in the same format at once, and combine them
into a single table - useful, for example, to read a folder of gff-files
with each file containing genes of a different genome.

## Usage

``` r
read_feats(files, .id = "file_id", format = NULL, parser = NULL, ...)

read_subfeats(files, .id = "file_id", format = NULL, parser = NULL, ...)

read_links(files, .id = "file_id", format = NULL, parser = NULL, ...)

read_sublinks(files, .id = "file_id", format = NULL, parser = NULL, ...)

read_seqs(
  files,
  .id = "file_id",
  format = NULL,
  parser = NULL,
  parse_desc = TRUE,
  ...
)
```

## Arguments

- files:

  files to reads. Should all be of same format. In many cases,
  compressed files (`.gz`, `.bz2`, `.xz`, or `.zip`) are supported.
  Similarly, automatic download of remote files starting with
  `http(s)://` or `ftp(s)://` works in most cases.

- .id:

  the column with the name of the file a record was read from. Defaults
  to "file_id". Set to "bin_id" if every file represents a different
  bin.

- format:

  specify a format known to gggenomes, such as `gff3`, `gbk`, ... to
  overwrite automatic determination based on the file extension (see
  [`def_formats()`](https://thackl.github.io/gggenomes/reference/def_formats.md)
  for full list).

- parser:

  specify the name of an R function to overwrite automatic determination
  based on format, e.g. `parser="read_tsv"`.

- ...:

  additional arguments passed on to the format-specific read function
  called down the line.

- parse_desc:

  turn `key=some value` pairs from `seq_desc` into `key`-named columns
  and remove them from `seq_desc`.

## Value

A gggenomes-compatible sequence, feature or link tibble

tibble with features

tibble with features

tibble with links

tibble with links

tibble with sequence information

## Functions

- `read_feats()`: read files as features mapping onto sequences.

- `read_subfeats()`: read files as subfeatures mapping onto other
  features

- `read_links()`: read files as links connecting sequences

- `read_sublinks()`: read files as sublinks connecting features

- `read_seqs()`: read sequence ID, description and length.

## Examples

``` r
# read genes/features from a gff file
read_feats(ex("eden-utr.gff"))
#> Reading 'gff3' with `read_gff3()`:
#> * file_id: eden-utr [/home/runner/work/_temp/Library/gggenomes/extdata/eden-utr.gff]
#> Harmonizing attribute names
#> • ID -> feat_id
#> • Name -> name
#> • Parent -> parent_ids
#> • Target -> target
#> Features read
#> # A tibble: 8 × 3
#>   source type                n
#>   <chr>  <chr>           <int>
#> 1 NA     CDS                 5
#> 2 NA     TF_binding_site     1
#> 3 NA     cDNA_match          1
#> 4 NA     exon                5
#> 5 NA     five_prime_UTR      1
#> 6 NA     gene                1
#> 7 NA     mRNA                5
#> 8 NA     three_prime_UTR     1
#> # A tibble: 20 × 15
#>    file_id  seq_id start   end strand type     feat_id introns parent_ids source
#>    <chr>    <chr>  <int> <int> <chr>  <chr>    <chr>   <list>  <list>     <chr> 
#>  1 eden-utr ctg123  1000  9000 +      gene     gene00… <NULL>  <chr [1]>  NA    
#>  2 eden-utr ctg123  1000  1012 +      TF_bind… tfbs00… <NULL>  <chr [1]>  NA    
#>  3 eden-utr ctg123  1050  9000 +      mRNA     mRNA00… <dbl>   <chr [1]>  NA    
#>  4 eden-utr ctg123  1050  9000 +      mRNA     mRNA00… <dbl>   <chr [1]>  NA    
#>  5 eden-utr ctg123  1300  9000 +      mRNA     mRNA00… <dbl>   <chr [1]>  NA    
#>  6 eden-utr ctg123  1300  9000 +      mRNA     mRNA00… <dbl>   <chr [1]>  NA    
#>  7 eden-utr ctg123  1300  1500 +      exon     exon00… <NULL>  <chr [1]>  NA    
#>  8 eden-utr ctg123  1050  1500 +      exon     exon00… <NULL>  <chr [2]>  NA    
#>  9 eden-utr ctg123  3000  3902 +      exon     exon00… <NULL>  <chr [2]>  NA    
#> 10 eden-utr ctg123  5000  5500 +      exon     exon00… <NULL>  <chr [3]>  NA    
#> 11 eden-utr ctg123  7000  9000 +      exon     exon00… <NULL>  <chr [3]>  NA    
#> 12 eden-utr ctg123  1201  7600 +      CDS      cds000… <dbl>   <chr [1]>  NA    
#> 13 eden-utr ctg123  1201  7600 +      CDS      cds000… <dbl>   <chr [1]>  NA    
#> 14 eden-utr ctg123  3301  7600 +      CDS      cds000… <dbl>   <chr [1]>  NA    
#> 15 eden-utr ctg123  3391  7600 +      CDS      cds000… <dbl>   <chr [1]>  NA    
#> 16 eden-utr ctg123  1050  9000 +      mRNA     mRNA00… <int>   <chr [1]>  NA    
#> 17 eden-utr ctg123  1050  1200 +      five_pr… feat_25 <NULL>  <chr [1]>  NA    
#> 18 eden-utr ctg123  1201  7600 +      CDS      cds000… <dbl>   <chr [1]>  NA    
#> 19 eden-utr ctg123  7601  9000 +      three_p… feat_30 <NULL>  <chr [1]>  NA    
#> 20 eden-utr ctg123  1050  9000 +      cDNA_ma… match0… <dbl>   <chr [1]>  NA    
#> # ℹ 5 more variables: score <chr>, phase <chr>, name <chr>, target <chr>,
#> #   geom_id <chr>


# read all gff files from a directory
read_feats(list.files(ex("emales/"), "*.gff$", full.names = TRUE))
#> Reading 'gff3' with `read_gff3()`:
#> * file_id: emales-ngaros [/home/runner/work/_temp/Library/gggenomes/extdata/emales//emales-ngaros.gff]
#> Harmonizing attribute names
#> • ID -> feat_id
#> Features read
#> # A tibble: 1 × 3
#>   source type              n
#>   <chr>  <chr>         <int>
#> 1 MFG    repeat_region     3
#> * file_id: emales-tirs [/home/runner/work/_temp/Library/gggenomes/extdata/emales//emales-tirs.gff]
#> Harmonizing attribute names
#> • ID -> feat_id
#> • Name -> name
#> Features read
#> # A tibble: 1 × 3
#>   source type              n
#>   <chr>  <chr>         <int>
#> 1 MFG    repeat_region    12
#> * file_id: emales [/home/runner/work/_temp/Library/gggenomes/extdata/emales//emales.gff]
#> Harmonizing attribute names
#> • ID -> feat_id
#> • Name -> name
#> • Note -> note
#> Features read
#> # A tibble: 1 × 3
#>   source type      n
#>   <chr>  <chr> <int>
#> 1 MFG    CDS     143
#> # A tibble: 158 × 17
#>    file_id     seq_id start   end strand type  feat_id introns parent_ids source
#>    <chr>       <chr>  <int> <int> <chr>  <chr> <chr>   <list>  <list>     <chr> 
#>  1 emales-nga… BVI_0… 11043 17065 NA     repe… nBVI_0… <NULL>  <chr [1]>  MFG   
#>  2 emales-nga… BVI_0…  7894 14280 NA     repe… nBVI_0… <NULL>  <chr [1]>  MFG   
#>  3 emales-nga… E4-10…  2854  9283 NA     repe… nE4-10… <NULL>  <chr [1]>  MFG   
#>  4 emales-tirs BVI_0…     1   473 +      repe… BVI_00… <NULL>  <chr [1]>  MFG   
#>  5 emales-tirs BVI_0… 26348 26820 -      repe… BVI_00… <NULL>  <chr [1]>  MFG   
#>  6 emales-tirs BVI_0…     1   488 +      repe… BVI_06… <NULL>  <chr [1]>  MFG   
#>  7 emales-tirs BVI_0… 26321 26808 -      repe… BVI_06… <NULL>  <chr [1]>  MFG   
#>  8 emales-tirs Cflag…     1  1081 +      repe… Cflag_… <NULL>  <chr [1]>  MFG   
#>  9 emales-tirs Cflag… 20231 21311 -      repe… Cflag_… <NULL>  <chr [1]>  MFG   
#> 10 emales-tirs E4-10…     1   319 +      repe… E4-10_… <NULL>  <chr [1]>  MFG   
#> # ℹ 148 more rows
#> # ℹ 7 more variables: score <chr>, phase <chr>, name <chr>, geom_id <chr>,
#> #   width <chr>, gc_content <chr>, note <chr>


# read remote files
# \donttest{
gbk_phages <- c(
  PSSP7 = paste0(
    "https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/",
    "000/858/745/GCF_000858745.1_ViralProj15134/",
    "GCF_000858745.1_ViralProj15134_genomic.gff.gz"
  ),
  PSSP3 = paste0(
    "https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/",
    "000/904/555/GCF_000904555.1_ViralProj195517/",
    "GCF_000904555.1_ViralProj195517_genomic.gff.gz"
  )
)
read_feats(gbk_phages)
#> Reading 'gff3' with `read_gff3()`:
#> * file_id: PSSP7 [https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/858/745/GCF_000858745.1_ViralProj15134/GCF_000858745.1_ViralProj15134_genomic.gff.gz]
#> Harmonizing attribute names
#> • ID -> feat_id
#> • Dbxref -> dbxref
#> • old-name -> old_name
#> • Name -> name
#> • Parent -> parent_ids
#> • Note -> note
#> Features read
#> # A tibble: 4 × 3
#>   source type                 n
#>   <chr>  <chr>            <int>
#> 1 RefSeq CDS                 58
#> 2 RefSeq gene                58
#> 3 RefSeq region               1
#> 4 RefSeq sequence_feature     1
#> * file_id: PSSP3 [https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/904/555/GCF_000904555.1_ViralProj195517/GCF_000904555.1_ViralProj195517_genomic.gff.gz]
#> Harmonizing attribute names
#> • ID -> feat_id
#> • Dbxref -> dbxref
#> • collection-date -> collection_date
#> • isolation-source -> isolation_source
#> • lab-host -> lab_host
#> • lat-lon -> lat_lon
#> • Name -> name
#> • Parent -> parent_ids
#> • Note -> note
#> Features read
#> # A tibble: 4 × 3
#>   source type           n
#>   <chr>  <chr>      <int>
#> 1 RefSeq CDS           53
#> 2 RefSeq gene          53
#> 3 RefSeq pseudogene     3
#> 4 RefSeq region         1
#> # A tibble: 228 × 36
#>    file_id seq_id     start   end strand type  feat_id introns parent_ids source
#>    <chr>   <chr>      <int> <int> <chr>  <chr> <chr>   <list>  <list>     <chr> 
#>  1 PSSP7   NC_006882…     1 45176 +      regi… NC_006… <NULL>  <chr [1]>  RefSeq
#>  2 PSSP7   NC_006882…  1079  1816 +      gene  gene-P… <int>   <chr [1]>  RefSeq
#>  3 PSSP7   NC_006882…  1079  1816 +      CDS   cds-YP… <NULL>  <chr [1]>  RefSeq
#>  4 PSSP7   NC_006882…  1845  2423 +      gene  gene-P… <int>   <chr [1]>  RefSeq
#>  5 PSSP7   NC_006882…  1845  2423 +      CDS   cds-YP… <NULL>  <chr [1]>  RefSeq
#>  6 PSSP7   NC_006882…  2637  2903 +      gene  gene-P… <int>   <chr [1]>  RefSeq
#>  7 PSSP7   NC_006882…  2637  2903 +      CDS   cds-YP… <NULL>  <chr [1]>  RefSeq
#>  8 PSSP7   NC_006882…  2900  3130 +      gene  gene-P… <int>   <chr [1]>  RefSeq
#>  9 PSSP7   NC_006882…  2900  3130 +      CDS   cds-YP… <NULL>  <chr [1]>  RefSeq
#> 10 PSSP7   NC_006882…  3127  3291 +      gene  gene-P… <int>   <chr [1]>  RefSeq
#> # ℹ 218 more rows
#> # ℹ 26 more variables: score <chr>, phase <chr>, name <chr>, dbxref <chr>,
#> #   gbkey <chr>, genome <chr>, mol_type <chr>, old_name <chr>,
#> #   gene_biotype <chr>, locus_tag <chr>, experiment <chr>, product <chr>,
#> #   protein_id <chr>, transl_table <chr>, gene <chr>, note <chr>,
#> #   geom_id <chr>, collection_date <chr>, isolation_source <chr>,
#> #   lab_host <chr>, lat_lon <chr>, strain <chr>, description <chr>, …
# }

# read sequences from a fasta file.
read_seqs(ex("emales/emales.fna"), parse_desc = FALSE)
#> Reading 'fasta' with `read_seq_len()`:
#> * file_id: emales [/home/runner/work/_temp/Library/gggenomes/extdata/emales/emales.fna]
#> # A tibble: 6 × 4
#>   file_id seq_id      seq_desc                                            length
#>   <chr>   <chr>       <chr>                                                <int>
#> 1 emales  BVI_008A    emale_type=EMALE01 is_typespecies=FALSE has_tir=TR…  26820
#> 2 emales  BVI_069     emale_type=EMALE01 is_typespecies=FALSE has_tir=TR…  26808
#> 3 emales  Cflag_017B  emale_type=EMALE01 is_typespecies=TRUE has_tir=TRUE  21311
#> 4 emales  E4-10_086   emale_type=EMALE01 is_typespecies=FALSE has_tir=TR…  20642
#> 5 emales  E4-10_112   emale_type=EMALE01 is_typespecies=FALSE has_tir=TR…  26856
#> 6 emales  RCC970_016B emale_type=EMALE01 is_typespecies=FALSE has_tir=TR…  20152

# read sequence info from a fasta file with `parse_desc=TRUE` (default). `key=value`
# pairs are removed from `seq_desc` and parsed into columns with `key` as name
read_seqs(ex("emales/emales.fna"))
#> Reading 'fasta' with `read_seq_len()`:
#> * file_id: emales [/home/runner/work/_temp/Library/gggenomes/extdata/emales/emales.fna]
#> # A tibble: 6 × 7
#>   file_id seq_id      seq_desc length emale_type is_typespecies has_tir
#>   <chr>   <chr>       <chr>     <int> <chr>      <lgl>          <lgl>  
#> 1 emales  BVI_008A    NA        26820 EMALE01    FALSE          TRUE   
#> 2 emales  BVI_069     NA        26808 EMALE01    FALSE          TRUE   
#> 3 emales  Cflag_017B  NA        21311 EMALE01    TRUE           TRUE   
#> 4 emales  E4-10_086   NA        20642 EMALE01    FALSE          TRUE   
#> 5 emales  E4-10_112   NA        26856 EMALE01    FALSE          TRUE   
#> 6 emales  RCC970_016B NA        20152 EMALE01    FALSE          TRUE   

# read sequence info from samtools/seqkit style index
read_seqs(ex("emales/emales.fna.seqkit.fai"))
#> Reading 'fai' with `read_fai()`:
#> * file_id: emales.fna.seqkit [/home/runner/work/_temp/Library/gggenomes/extdata/emales/emales.fna.seqkit.fai]
#> # A tibble: 6 × 7
#>   file_id           seq_id     seq_desc length emale_type is_typespecies has_tir
#>   <chr>             <chr>      <chr>     <int> <chr>      <lgl>          <lgl>  
#> 1 emales.fna.seqkit BVI_008A   NA        26820 EMALE01    FALSE          TRUE   
#> 2 emales.fna.seqkit BVI_069    NA        26808 EMALE01    FALSE          TRUE   
#> 3 emales.fna.seqkit Cflag_017B NA        21311 EMALE01    TRUE           TRUE   
#> 4 emales.fna.seqkit E4-10_086  NA        20642 EMALE01    FALSE          TRUE   
#> 5 emales.fna.seqkit E4-10_112  NA        26856 EMALE01    FALSE          TRUE   
#> 6 emales.fna.seqkit RCC970_01… NA        20152 EMALE01    FALSE          TRUE   

# read sequence info from multiple gff file
read_seqs(c(ex("emales/emales.gff"), ex("emales/emales-tirs.gff")))
#> Reading 'gff3' with `read_seq_len()`:
#> * file_id: emales [/home/runner/work/_temp/Library/gggenomes/extdata/emales/emales.gff]
#> * file_id: emales-tirs [/home/runner/work/_temp/Library/gggenomes/extdata/emales/emales-tirs.gff]
#> # A tibble: 12 × 4
#>    file_id     seq_id      seq_desc length
#>    <chr>       <chr>       <chr>     <dbl>
#>  1 emales      BVI_008A    NA        26820
#>  2 emales      BVI_069     NA        26808
#>  3 emales      Cflag_017B  NA        21311
#>  4 emales      E4-10_086   NA        20642
#>  5 emales      E4-10_112   NA        26856
#>  6 emales      RCC970_016B NA        20152
#>  7 emales-tirs BVI_008A    NA        26820
#>  8 emales-tirs BVI_069     NA        26808
#>  9 emales-tirs Cflag_017B  NA        21311
#> 10 emales-tirs E4-10_086   NA        20642
#> 11 emales-tirs E4-10_112   NA        26856
#> 12 emales-tirs RCC970_016B NA        20152
```
