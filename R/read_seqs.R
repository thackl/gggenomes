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
read_seqs <- function(file, col_names=c("seq_id", "seq_desc", "length")){
  peek <- readChar(file, 1)
  if(peek == ">"){ # fasta
    # Note base::system.file b/c devtools::system.file looks at "package/inst"
    # as this would become toplevel after install. The exec folder and its
    # script are a special case that are always toplevel
    seq_len <- base::system.file("exec/seq-len", package="gggenomes")
    read_tsv(pipe(str_glue("{seq_len} {file}")), col_names = col_names, col_types="ccn")
  }else{
    df <- read_tsv(file, col_types="cn---", col_names=F) %>%
      separate(1, into=c("seq_id", "seq_desc"), fill="right", sep=" ", extra="merge")
    if(length(col_names) > ncol(df)){
      rlang::warn("Too many col_names, ignoring extra ones")
      col_names <- col_names[seq_along(df)]
    }
    colnames(df)[seq_along(col_names)] <- col_names
    df
  }
}
