#' Swap query and subject
#'
#' Swap query and subject columns in a table read with [read_feats()] or
#' [read_links()], for example, from blast searches. Swaps columns with
#' name/name2, such as 'seq_id/seq_id2', 'start/start2', ...
#'
#' @param x tibble with query and subject columns
#' @export
#' @return tibble with swapped query/subject columns
#' @examples
#' feats <- tribble(
#'  ~seq_id, ~seq_id2, ~start, ~end, ~strand, ~start2, ~end2, ~evalue,
#'  "A", "B", 100, 200, "+", 10000, 10200, 1e-5
#' )
#' # make B the query
#' swap_query(feat)
swap_query <- function(x){
  # for every pair seq_id/seq_id2, name/name2 > name2/name
  n <- names(x)
  m <- str_subset(n, "\\D2") %>% str_remove("2$") %>% intersect(n)
  m2 <- paste0(m, "2")
  i <- which(n %in% m)
  i2 <- which(n %in% m2)
  inform(c("swapping", comma(m, collapse='  '), comma(m2, collapse=' ')))
  x[c(i, i2)] <- x[c(i2, i)]
  x
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
#'
#' @param context a file format context defined in `gggenomes_global$file_formats`
#' @return dictionarish vector of file formats with recognized extensions as names
#' @examples
#' # vector of defined zip formats and recognized extensions as names
#' file_formats("zips")
#' @eval file_formats_rd()
file_formats <- function(context){
  ff <- gggenomes_global$file_formats
  if(!context %in% names(ff)){
    abort(c(
      str_glue("Unknown file format context '{context}'.\nDefined families are:"),
      names(ff)
    ))
  }
  ff[[context]]
}

#' Defined file extensions and associated formats
#'
#' @inheritParams file_formats
#' @return vector of file extensions with formats as names
#' @examples
#' # vector of zip-context file extensions and format names
#' file_exts("zips")
file_exts <- function(context){
  f <- file_formats(context)
  set_names(names(f), f)
}

#' File format from suffix
#' @param x a vector of file extensions
#' @param context a file format context defined in [file_formats()]
#' @return a vector of formats with extensions as names
#' @examples
#' ext_to_format(c("gff", "txt", "FASTA"), "feats")
ext_to_format <- function(x, context){
  x <- str_to_lower(x)
  if(is_dictionaryish(context))
    context[x]
  else
    file_formats(context)[x]
}

file_strip_zip <- function(file, ext = names(file_formats("zips"))){
  ext <- paste0("\\.", ext, "$", collapse="|")
  str_remove(file, ext)
}

file_ext <- function(file, pattern = "(?<=\\.)[^.]+$", ignore_zip = TRUE){
  if(ignore_zip)
    file <- file_strip_zip(file)
  str_extract(file, pattern)
}

file_name <- function(file, pattern = "\\.[^.]+$", ignore_zip = TRUE){
  if(ignore_zip)
    file <- file_strip_zip(file)
  str_remove(basename(file), pattern)
}

file_format <- function(file, context, allow_na = FALSE){
  ext <- file_ext(file)
  format <- ext_to_format(ext, context)
  if(!allow_na && any(is.na(format))){
    bad <- file[is.na(format)]
    names(bad) <- rep("x", length(bad))
    good <- file_formats("feats") %>%
      enframe(name = "ext", value = "format") %>%
      chop(ext) %>% mutate(ext = map_chr(ext, comma)) %>% format()
    abort(c(str_glue('Bad extention for file format context "{context}"'), bad,
      i="Recognized formats/extensions:", good[-(1:3)]))
  }
  set_names(format, file)
}

file_id <- function(file){
  vctrs::vec_as_names(file_name(file), repair="unique")
}

file_format_unique <- function(files, context, allow_duplicates = FALSE){
  fmt <- unique(file_format(files, context))
  if(!allow_duplicates && length(fmt) > 1)
    abort(c("All files need the same format.", i="Got mix of:", unname(fmt)))
  fmt
}

#' Add a unique name to files
#'
#' Given a vector of file paths, add a unique labels based on the filename as
#' vector names
file_label <- function(file){
  i <- which(!have_name(file))
  names(file)[i] <- file_id(file[i])
  file
}


file_is_zip <- function(file, ext = names(file_formats("zips"))){
  pattern <- paste0("\\.", ext, "$", collapse="|")
  str_detect(file, pattern)
}

file_is_url <- function(file){
  str_detect(file, "^((http|ftp)s?|sftp)://")
}

file_formats_rd <- function(){
  ff <- gggenomes_global$file_formats %>%
    map_df(.id="context", function(x){
      enframe(x, "extension", "format") %>% group_by(format) %>%
        summarize(extension = comma(extension), .groups="drop")
    })
  ff <- mutate(ff, context = ifelse(duplicated(context), "", context))

  ff <- str_c(sep = "\n",
      "@section Defined Contexts, Formats and Extensions:",
      "\\preformatted{",
      #sprintf("%-8s %-7s  %s", "Context", "Format", "Extensions"),
      str_c(collapse = "\n",
            str_glue_data(ff, '{sprintf("%-8s", context)} ',
                    '{sprintf("%-7s", format)}  [{extension}]')),
      "}"
      )
  ff
}
