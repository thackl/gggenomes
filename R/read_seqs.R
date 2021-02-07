#' Read a sequence index
#'
#' Read ID, description and length for each sequence from common formats
#' including FASTA, samtools/seqkit FASTA index files, and GFF3. Default columns
#' are seq_id, seq_desc and length.
#'
#' @importFrom readr read_tsv
#' @param file fasta or .fai/.seqkit.fai fasta index
#' @param parse_desc remove `key=some value` pairs from `seq_desc` and turn them
#'   into `key`-named columns.
#' @export
#' @return A gggenomes-compatible sequence tibble
#' @describeIn read_seqs read seqs from files with automatic format detection
#' @examples
#' # read sequences from a fasta file.
#' read_seqs(ex("emales/emales.fna"), parse_desc=FALSE)
#'
#' # read sequences from a fasta file with `parse_desc=TRUE` (default). `key=value`
#' # pairs are removed from `seq_desc` and parsed into columns with `key` as name
#' read_seqs(ex("emales/emales.fna"))
#'
#' # from samtools/seqkit style index
#' read_seqs(ex("emales/emales.fna.seqkit.fai"))
#'
#' # from multiple gff file
#' read_seqs(c(ex("emales/emales.gff"), ex("emales/emales-tirs.gff")))
#'
read_seqs <- function(files, format=NULL, .id="file_id", parse_desc=TRUE, ...){
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

  if(parse_desc){
    seqs <- mutate(seqs, parse_desc(seq_desc))
  }

  seqs
}

parse_desc <- function(x, pattern="\\s*(\\S+)=\\{?([^=]+?)(\\s|\\}|$)"){
  m <- str_match_all(x, pattern) # create  list of match matrices
  y <- map_df(m, function(.x){
    # if x was NA, all match values are NA - return empty tibble with one row
    if(any(is.na(.x[,2]))) return(tibble(.rows=1))
    # else return tibble with keys as names
    .x[,3] %>% as.list %>% set_names(.x[,2]) %>% as_tibble
  })

  # if key has the name of a reserved column, rename it so we don't overwrite
  rename_i <- names(y) %in% qc(file_id, seq_id, seq_desc, length)
  names(y)[rename_i] <- paste0("seq_desc_", names(y)[rename_i])

  # remove key=value data from seq_desc and turn resulting emtpy "" into NA
  z <- str_remove_all(x, pattern)
  z[z==""] <- NA

  mutate(y, seq_desc=z, .before=1)
}

#' @describeIn read_seqs read seqs from a single file in fasta, gbk or gff3 format.
#' @export
read_seq_len <- function(file, col_names = def_names("seq_len"),
    col_types = def_types("seq_len"), ...){
  # Note base::system.file b/c devtools::system.file looks at "package/inst"
  # as this would become toplevel after install. The exec folder and its
  # script are a special case that are always toplevel
  seq_len <- base::system.file("exec/seq-len", package="gggenomes")
  read_tsv(pipe(str_glue("{seq_len} {file}")), col_names = col_names, col_types=col_types, ...)

}

#' @describeIn read_seqs read seqs from a single file in seqkit/samtools fai format.
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
