#' @export
#' @describeIn read_tracks read sequence ID, description and length.
#' @examples
#' # reads sequence index from a fasta file
#' read_seqs(ex("emales/emales.fna"))
#'
#'
#' # from samtools/seqkit style index
#' read_seqs(ex("emales/emales.fna.seqkit.fai"))
#'
#'
#' # from multiple gff file
#' read_seqs(c(ex("emales/emales.gff"), ex("emales/emales-tirs.gff")))
read_seqs <- function(files, .id="file_id", format=NULL, parser=NULL, ...){
  read_context(files, "seqs", .id=.id, format=format, parser=parser, ...)
}


#' Read sequence index
#'
#' @describeIn read_seq_len read seqs from a single file in fasta, gbk or gff3 format.
#' @export
read_seq_len <- function(file, col_names = def_names("seq_len"),
    col_types = def_types("seq_len"), ...){
  # Note base::system.file b/c devtools::system.file looks at "package/inst"
  # as this would become toplevel after install. The exec folder and its
  # script are a special case that are always toplevel
  seq_len <- base::system.file("exec/seq-len", package="gggenomes")
  read_tsv(pipe(str_glue("{seq_len} {file}")), col_names = col_names, col_types=col_types, ...)

}

#' @describeIn read_seq_len read seqs from a single file in seqkit/samtools fai format.
#' @export
read_fai <- function(file, col_names=def_names("fai"),
    col_types=def_types("fai"), ...){
  df <- read_tsv(file, col_types, col_names=F, ...) %>%
    separate(1, into=c("seq_id", "seq_desc"), fill="right", sep=" ", extra="merge")
  if(length(col_names) > ncol(df)){
    rlang::warn("Too many col_names, ignoring extra ones")
    col_names <- col_names[seq_along(df)]
  }
  colnames(df)[seq_along(col_names)] <- col_names
  df
}
