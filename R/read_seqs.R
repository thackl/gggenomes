#' Read a sequence index
#'
#' Read ID, description and length for each sequence from a fasta file or from a
#' samtools/seqkit fasta index file. Default columns are "seq_id", "seq_desc"
#' and "length".
#'
#' @importFrom readr read_tsv
#' @param file fasta or .fai/.seqkit.fai fasta index
#' @param col_names custom column names
#' @export
#' @return tibble
#' @examples
#' read_seqs("foo.fa")
#' read_seqs("foo.fa.fai")        # samtools style index, no description
#' read_seqs("foo.fa.seqkit.fai") # seqkit style with description
#' # custom col_names
#' read_seqs("foo.fa.fai", col_names=c("contig_id", "contig_desc", "len"))
read_seqs <- function(files, format=NULL, .id="file_id", ...){
  if(any(map_lgl(files, is_connection))){
    warn("Using connections instead of paths to files can lead to unexpected behaviour")
    is_connection(files)
      files <- list(files) # weird things happen to pipes in vectors
  }

  # infer file format from suffix
  format <- (format %||% file_format_unique(files, "seqs"))

  if(format == 'ambigious'){
    abort(str_glue('Ambigious file extension(s): "', comma(unique(file_ext(files))),
                   '".\nPlease specify `format` explicitly'))
  }

  # for unnamed files, infer name from filename (used as file_id/bin_id)
  files <- file_label(files)

  # map_df .id = bin_id
  inform(str_glue("Reading as {format}:"))
  seqs <- map2_df(files, names(files), read_format, .id=.id, format, ...)

  seqs
}

#' @rdname read_seqs
#' @export
read_seq_len <- function(file, col_names = def_names("seq_len"),
    col_types = def_types("seq_len"), ...){
  # Note base::system.file b/c devtools::system.file looks at "package/inst"
  # as this would become toplevel after install. The exec folder and its
  # script are a special case that are always toplevel
  seq_len <- base::system.file("exec/seq-len", package="gggenomes")
  read_tsv(pipe(str_glue("{seq_len} {file}")), col_names = col_names, col_types=col_types, ...)

}

#' @rdname read_seqs
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
