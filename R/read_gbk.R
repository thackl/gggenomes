#' Read genbank files
#'
#' Genbank flat files (.gb/.gbk/.gbff) and their ENA and DDBJ equivalents have a
#' particularly gruesome format. That's why [read_gbk()] is just a wrapper
#' around a Perl-based `gb2gff` converter shipped with this package and
#' [read_gff3()].
#'
#' @importFrom readr read_tsv
#' @inheritParams read_gff3
#' @export
#' @return tibble
read_gbk <- function(file, sources=NULL, types=NULL){
  inform("Converting to GFF3")
  gb2gff <- base::system.file("exec/gb2gff", package="gggenomes")
  read_gff3(pipe(str_glue("{gb2gff} -S {file}")), sources=sources, types=types)
}
