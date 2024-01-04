#' Read files in various standard formats (FASTA, GFF3, GBK, BED, BLAST, ...) into track tables
#'
#' Convenience functions to read sequences, features or links from various
#' bioinformatics file formats, such as FASTA, GFF3, Genbank, BLAST tabular
#' output, etc. See [def_formats()] for full list. File formats and the
#' corresponding read-functions are automatically determined based on file
#' extensions. All these functions can read multiple files in the same format at
#' once, and combine them into a single table - useful, for example, to read a
#' folder of gff-files with each file containing genes of a different genome.
#'
#' @name read_tracks
#' @inheritParams read_context
#' @return A gggenomes-compatible sequence, feature or link tibble
NULL


#' Read files in different contexts
#'
#' Powers [read_seqs()], [read_feats()], [read_links()]
#' @param files files to reads. Should all be of same format. In many cases,
#'   compressed files (`.gz`, `.bz2`, `.xz`, or `.zip`) are supported.
#'   Similarly, automatic download of remote files starting with `http(s)://` or
#'   `ftp(s)://` works in most cases.
#' @param .id the column with the name of the file a record was read from.
#'   Defaults to "file_id". Set to "bin_id" if every file represents a different
#'   bin.
#' @param format specify a format known to gggenomes, such as `gff3`, `gbk`, ...
#'   to overwrite automatic determination based on the file extension (see
#'   [def_formats()] for full list).
#' @param parser specify the name of an R function to overwrite automatic
#'   determination based on format, e.g. `parser="read_tsv"`.
#' @param ... additional arguments passed on to the format-specific read
#'   function called down the line.
#' @param context the context ("seqs", "feats", "links") in which a given format
#'   should be read.
#' @describeIn read_context bla keywords internal
read_context <- function(files, context, .id="file_id", format=NULL, parser=NULL, ...){
  if(is_connection(files))
    files <- list(files) # weird things happen to pipes in vectors

  # for unnamed files, infer name from filename (used as file_id/bin_id)
  files <- file_label(files)

  parser <- parser %||% file_parser(files, context=context, format=format, require_unique=T)
  # map_df .id = bin_id
  inform(str_glue("Reading '{names(parser)}' with `{parser}()`:"))
  x <- purrr::map2_df(files, names(files), .id=.id, parser=parser, ...,
               .f=function(file, name, parser, ...){
                 inform(str_glue("* {.id}: {name} [{file}]"))
                 exec(parser, file, ...)
               })

  x
}

read_ambigious <- function(file, ...){
  abort(c("Ambigious file extension, please specify format or parser explicitly"), file)
}

# file: vec of files
# context: vec of context
# single file/context is recycled to match multiple context/files if given
# format:  force this format regardless of file extension
file_parser <- function(file, context=NULL, format=NULL, require_unique=FALSE){
  format <- format %||% def_formats(file, context=context)
  parser <- def_parser(format, context=context) %>% set_names(format)

  if(require_unique){
    p <- unique(parser)
    if(length(p) > 1)
      abort(c("All files need the same format/parser.", i="Got mix of:", unname(p)))
    parser <- parser[1] # unique(parser) strips names
  }
  parser
}

#' Defined file formats and extensions
#'
#' For seamless reading of different file formats, gggenomes uses a mapping of
#' known formats to associated file extensions and contexts in which the
#' different formats can be read. The notion of context allows one to read
#' different information from the same format/extension. For example, a gbk file
#' holds both feature and sequence information. If read in "feats" context
#' `read_feats("*.gbk")` it will return a feature table, if read in "seqs"
#' context `read_seqs("*.gbk")`, a sequence index.
#'
#' @param file a vector of file names
#' @param ext a vector of file extensions
#' @param context a vector of file contexts defined in
#'   `gggenomes_global$def_formats`
#' @param parser a vector of file parsers defined in
#'   `gggenomes_global$def_formats`
#' @param allow_na boolean
#' @return dictionarish vector of file formats with recognized extensions as
#'   names
#' @export
#' @examples
#' # vector of defined zip formats and recognized extensions as names
#' # format of file
#' def_formats("foo.fa")
#'
#' # formats associated with each extension
#' def_formats(ext=c("fa", "gff"))
#'
#' # all formats/extensions that can be read in seqs context; includes formats
#' # that are defined for context=NA, i.e. that can be read in any context.
#' def_formats(context="seqs")
#' @eval def_formats_rd()
def_formats <- function(file=NULL, ext=NULL, context=NULL, parser=NULL, allow_na=FALSE){
  if(!is.null(file)){
    ext <- c(file_ext(file), ext)
  }

  ff <- filter_def_formats(context=context, parser=parser) %>% tidyr::unchop(ext)

  format <- tibble::deframe(select(ff, ext, format))
  if(!is.null(ext))
    format <- format[ext]

  if(!allow_na && any(is.na(format))){
    bad <- ext[is.na(format)]
    names(bad) <- rep("x", length(bad))
    good <- def_formats(context=context, parser=parser) %>%
      tibble::enframe(name = "ext", value = "format") %>%
      tidyr::chop(ext) %>% mutate(ext = purrr::map_chr(ext, comma)) %>% format()
    abort(c(str_glue('Unknown extention(s):'),
            i=str_glue("in context: {context}"),
            i=str_glue("with parser: {parser}"),
            bad,
            i="Recognized formats/extensions for given context/parser:",
            good[-(1:3)]))
  }
  format
}

#' Default column names and types for defined formats
#'
#' Intended to be used in [readr::read_tsv()]-like functions that accept a
#' `col_names` and a `col_types` argument.
#'
#' @export
#' @param format specify a format known to gggenomes, such as `gff3`, `gbk`, ...
#' @return a vector with default column names for the given format
#' @eval def_names_rd()
#' @describeIn def_names default column names for defined formats
#' @examples
#' # read a blast-tabular file with read_tsv
#' readr::read_tsv(ex("emales/emales-prot-ava.o6"), col_names=def_names("blast"))
def_names <- function(format){
  ff <- gggenomes_global$def_names
  if(!format %in% names(ff)){
    abort(c(
      str_glue("No default col_names defined for format '{format}'.\nDefined formats are:"),
      names(ff)
    ))
  }
  ff[[format]]
}

#' @describeIn def_names default column types for defined formats
#' @export
#' @return a vector with default column types for the given format
def_types <- function(format){
  ff <- gggenomes_global$def_types
  if(!format %in% names(ff)){
    abort(c(
      str_glue("No default col_types defined for format '{format}'.\nDefined formats are:"),
      names(ff)
    ))
  }
  ff[[format]]
}

def_parser <- function(format, context=NULL){
  context <- context %||% NA

  # recycle format & context to same length
  x <- tibble(format=format, context=context)

  # for each format/context combo, get parser
  pp <- purrr::pmap_chr(x, function(format, context){
    r <- filter_def_formats(format=format, context=context) %>% pull(parser)
    if(!length(r) || is.na(r))
      abort(str_glue("No predefined parser for: `format={format}, context={context}`"))
    r
  })
  pp
}

file_strip_zip <- function(file, ext = qc(bz2,gz,xz,zip)){
  ext <- paste0("\\.", ext, "$", collapse="|")
  stringr::str_remove(file, ext)
}

file_ext <- function(file, pattern = "(?<=\\.)[^.]+$", ignore_zip = TRUE){
  if(ignore_zip)
    file <- file_strip_zip(file)
  stringr::str_extract(file, pattern)
}

file_name <- function(file, pattern = "\\.[^.]+$", ignore_zip = TRUE){
  if(ignore_zip)
    file <- file_strip_zip(file)
  stringr::str_remove(basename(file), pattern)
}

file_id <- function(file){
  vctrs::vec_as_names(file_name(file), repair="unique")
}

#' Add a unique name to files
#'
#' Given a vector of file paths, add a unique labels based on the filename as
#' vector names
#'
#' @param file vector of files
file_label <- function(file){
  i <- which(!have_name(file))
  names(file)[i] <- file_id(file[i])
  file
}

file_is_zip <- function(file, ext = qc(bz2,gz,xz,zip)){
  pattern <- paste0("\\.", ext, "$", collapse="|")
  str_detect(file, pattern)
}

file_is_url <- function(file){
  str_detect(file, "^((http|ftp)s?|sftp)://")
}

is_connection <- function(x) inherits(x, "connection")


# filter but keep fallback parser for context=NA
filter_def_formats <- function(ff, format=NULL, context=NULL, parser=NULL){
  ff <- gggenomes_global$def_formats
  if(!is.null(format)){
    ff <- filter(ff, format %in% !!format)
  }

  if(!is.null(context) || !is.null(parser)){
    ff <- tidyr::unchop(ff, c(context, parser))
    if(!is.null(context)){
      # context=NA defines fallback parser which is always last in arrange
      ff <- ff %>% dplyr::group_by(format) %>%
        dplyr::filter(context %in% !!context | is.na(context)) %>%
        dplyr::arrange(context, .by_group = TRUE) %>% slice_head(n=1)
    }
    if(!is.null(parser))
      ff <- dplyr::filter(ff, parser %in% !!parser)
  }
  ff
}

def_formats_rd <- function(){
  stringr::str_c(collapse = "\n", c(
    "@section Defined formats, extensions, contexts, and parsers:",
    "\\preformatted{",
    testthat::capture_output(as.data.frame(gggenomes_global$def_formats), print=TRUE, width=120),
    "}"))
}

def_names_rd <- function(){
  ns <- gggenomes_global$def_names
  ts <- gggenomes_global$def_types
  stringr::str_c(sep = "\n",
        "@section Defined formats, column types and names:",
        "\\preformatted{",
        paste0(purrr::map(names(ns),
                   ~sprintf("  %-10s %-15s %s", .x, ts[[.x]], comma(ns[[.x]]))), collapse="\n"),
        "}"
  )
}
