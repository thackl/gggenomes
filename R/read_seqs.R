#' @param parse_desc turn `key=some value` pairs from `seq_desc` into `key`-named
#'   columns and remove them from `seq_desc`.
#' @export
#' @describeIn read_tracks read sequence ID, description and length.
#' @examples
#' # read sequences from a fasta file.
#' read_seqs(ex("emales/emales.fna"), parse_desc=FALSE)
#'
#' # read sequence info from a fasta file with `parse_desc=TRUE` (default). `key=value`
#' # pairs are removed from `seq_desc` and parsed into columns with `key` as name
#' read_seqs(ex("emales/emales.fna"))
#'
#' # read sequence info from samtools/seqkit style index
#' read_seqs(ex("emales/emales.fna.seqkit.fai"))
#'
#' # read sequence info from multiple gff file
#' read_seqs(c(ex("emales/emales.gff"), ex("emales/emales-tirs.gff")))
#'
read_seqs <- function(files, .id="file_id", format=NULL, parser=NULL,
    parse_desc=TRUE, ...){
  seqs <- read_context(files, "seqs", .id=.id, format=format, parser=parser, ...)

  if(parse_desc && has_name(seqs, "seq_desc")){
    seqs <- mutate(seqs, parse_desc(.data$seq_desc))
  }

  seqs
}

#' @importFrom utils type.convert
parse_desc <- function(x, pattern="\\s*\\[?(\\S+)=\\{?([^=]+?)(\\s|\\}|\\]|$)"){
  m <- str_match_all(x, pattern) # create  list of match matrices
  y <- purrr::map_df(m, function(.x){
    # if x was NA, all match values are NA - return empty tibble with one row
    if(nrow(.x) < 1 || any(is.na(.x[,2]))) return(tibble(.rows=1))
    # else return tibble with keys as names
    .x[,3] %>% as.list %>% set_names(.x[,2]) %>% as_tibble
  }) %>% mutate(across(everything(), type.convert, as.is=TRUE))

  # if key has the name of a reserved column, rename it so we don't overwrite
  rename_i <- names(y) %in% c("file_id", "seq_id", "seq_desc", "length")
  names(y)[rename_i] <- paste0("seq_desc_", names(y)[rename_i])

  # remove key=value data from seq_desc and turn resulting emtpy "" into NA
  z <- stringr::str_remove_all(x, pattern)
  z[z==""] <- NA

  mutate(y, seq_desc=z, .before=1)
}

#' Read sequence index
#'
#' @describeIn read_seq_len read seqs from a single file_name in fasta, gbk or gff3 format.
#' @param file with sequence length information
#' @export
read_seq_len <- function(file) {
  # Read the first line to determine format
  first <- readr::read_lines(file, n_max=1)

  # Initialize output
  output <- NULL

  # fasta
  if (stringr::str_starts(first, ">")) {
    output <- tibble::tibble(raw=readr::read_lines(file)) %>%
                # seq_number is only there to maintain order of the sequences (this might not be relevant)
                dplyr::mutate(
                  header=dplyr::if_else(stringr::str_starts(.data$raw, ">"), stringr::str_remove(.data$raw,">"), NA_character_),
                  seq_number=cumsum(!is.na(.data$header)),
                  bp=str_length(.data$raw)
                ) %>%
                tidyr::fill(.data$header, .direction="down") %>%
                dplyr::filter(!stringr::str_starts(.data$raw,">")) %>%
                dplyr::group_by(.data$seq_number, .data$header) %>%
                dplyr::summarize(length=sum(.data$bp)) %>%
                tidyr::separate(.data$header, into=c("seq_id","seq_desc"), sep=" ", extra="merge", fill = "right") %>%
                dplyr::ungroup() %>%
                dplyr::select(-.data$seq_number)
  }

  # gbk
  else if (stringr::str_starts(first, "LOCUS")) {
    output <- tibble::tibble(raw=readr::read_lines(file)) %>%
                dplyr::filter(stringr::str_starts(.data$raw, "LOCUS")) %>%
                tidyr::separate(.data$raw, into=c("tag", "seq_id", "length"), extra="drop", convert=TRUE) %>%
                dplyr::transmute(.data$seq_id, seq_desc=NA_character_, .data$length)
  }

  # gff
  else if (stringr::str_starts(first, "##gff")) {
    output <- tibble::tibble(raw=readr::read_lines(file)) %>%
                dplyr::filter(stringr::str_starts(.data$raw, "##sequence-region")) %>%
                tidyr::separate(.data$raw, into=c("_", "seq_id", "start", "end"), sep=" ", convert=TRUE) %>%
                dplyr::transmute(.data$seq_id, seq_desc=NA_character_, length=.data$end-.data$start+1)
  }

  # Unknown format
  else {
    stop("Unknown format, can read fasta, gbk and gff with proper ##gff-version and ##sequence-region directives\n")
  }

  return(output)
}

#' @describeIn read_seq_len read seqs from a single file in seqkit/samtools fai format.
#' @export
read_fai <- function(file, col_names=def_names("fai"),
    col_types=def_types("fai"), ...){
  df <- readr::read_tsv(file, col_types, col_names=F, ...) %>%
    separate(1, into=c("seq_id", "seq_desc"), fill="right", sep=" ", extra="merge")
  if(length(col_names) > ncol(df)){
    rlang::warn("Too many col_names, ignoring extra ones")
    col_names <- col_names[seq_along(df)]
  }
  colnames(df)[seq_along(col_names)] <- col_names
  df
}
