#' Read .gff files
#'
#' Uses `rtracklayer::import`. Assume contig IDs are unique amongst files if
#' genome_ids are not provided
#'
#' @param gff_files to read
#' @param genome_ids for each file. Only necessary of contig_ids are not unique
#' among different genomes.
#' @export
#' @return tibble
read_gffs <- function(gff_files, genome_ids = NULL){
  if (!requireNamespace("rtracklayer", quietly = TRUE)) {
    stop("Reading .gffs requires package 'rtracklayer' to be installed.",
         call. = FALSE)
  }
  if(!is.null(genome_ids)) names(gff_files) <- genome_ids

  TODO("list types, suggest filter")
  map_df(gff_files, function(gff){
    as_tibble(rtracklayer::import(gff)) %>%
      mutate_if(is.factor, as.character)
  })
}

#' Read genome_ids, contig_ids and contig lengths from .gff files.
#'
#' Parses `##sequence-region` annotation using `grep`. `rtracklayer` ignores
#' those lines.
#'
#' @param genome_ids to use with each file. If `NULL` infer from file name.
#' @export
#' @return A tibble with columns: genome_id, contig_id, length.
 read_gffs_as_contigs <- function(gff_files, genome_ids = NULL){
    data <- map(gff_files, function(gff){
        data <- read_table(pipe(paste('grep ^##sequence-region ', gff)), col_names = c("contig_id", "from", "to"), col_types = "-cnn") %>%
        mutate(length = to - from + 1) %>%
            select(-from, -to)
    })

    # genome ids
    if(is.null(genome_ids)){
        genome_ids <- sapply(gff_files, basename) %>%
            stringr::str_replace(".gff", "")
        if(any(duplicated(genome_ids))) stop("Filenames gave non-unique genome IDs, use `genome_id=` to specify manually")
    }
    names(data) <- genome_ids

    TODO("print summary info: read X genomes with y contigs, ...")

    # bind
    bind_rows(data, .id="genome_id") %>% as_tibble
}

#' read a .paf file (minimap/minimap2). Only the first 12 canonical
#' columns. Ignores tagged extra fields.
#'
#' @inheritParams thacklr::read_paf
#' @importFrom readr read_tsv
#' @importFrom thacklr  read_paf
#' @export
#' @return tibble
read_paf <- function(file, max_tags=20){
  thacklr::read_paf(file, max_tags) %>%
    rename(
      seq_id1=query_name, seq_id2=target_name,
      start1=query_start, start2=target_start,
      end1=query_end, end2=target_end,
      length1=query_length, length2=target_length
    )
}


#' Defined file formats and recognized suffixes
#'
#' @param family a file format family defined in `gggenomes_global$file_formats`
#' @return dictionarish vector of file formats with recognized suffixes as names
#' @examples
#' # vector of defined zip formats and recognized suffixes as names
#' file_formats("zips")
file_formats <- function(family){
  ff <- gggenomes_global$file_formats
  if(!family %in% names(ff)){
    abort(c(
      str_glue("Unknown file format family '{family}'.\nDefined families are:"),
      names(ff)
    ))
  }
  ff[[family]]
}

#' Defined file suffixes and associated formats
#'
#' @inheritParams file_formats
#' @return vector of file suffixes with formats as names
#' @examples
#' # vector of zip-family file suffixes and format names
#' file_suffixes("zips")
file_suffixes <- function(family){
  f <- file_formats(family)
  set_names(names(f), f)
}

#' File format from suffix
#' @param x a vector of file suffixes
#' @param family a file format family defined in [file_formats()]
#' @return a vector of formats with suffixes as names
#' @examples
#' suffix_to_format(c("gff", "txt", "FASTA"), "feats")
suffix_to_format <- function(x, family){
  x <- str_to_lower(x)
  if(is_dictionaryish(family))
    family[x]
  else
    file_formats(family)[x]
}

file_strip_zip <- function(file, suffix = names(file_formats("zips"))){
  suffix <- paste0("\\.", suffix, "$", collapse="|")
  str_remove(file, suffix)
}

file_suffix <- function(file, pattern = "(?<=\\.)[^.]+$", ignore_zip = TRUE){
  if(ignore_zip)
    file <- file_strip_zip(file)
  str_extract(file, pattern)
}

file_name <- function(file, pattern = "\\.[^.]+$", ignore_zip = TRUE){
  if(ignore_zip)
    file <- file_strip_zip(file)
  str_remove(basename(file), pattern)
}

file_format <- function(file, family, allow_na = FALSE){
  suffix <- file_suffix(file)
  format <- suffix_to_format(suffix, family)
  if(!allow_na && any(is.na(format))){
    bad <- file[is.na(format)]
    names(bad) <- rep("x", length(bad))
    good <- file_formats("feats") %>%
      enframe(name = "suffix", value = "format") %>%
      chop(suffix) %>% mutate(suffix = map_chr(suffix, comma)) %>% format()
    abort(c(str_glue('Bad suffix for file format family "{family}"'), bad,
      i="Recognized formats/suffixes:", good[-(1:3)]))
  }
  set_names(format, file)
}

file_id <- function(file){
  vctrs::vec_as_names(file_name(file), repair="unique")
}

file_format_unique <- function(files, family, allow_duplicates = FALSE){
  fmt <- unique(file_format(files, family))
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


file_is_zip <- function(file, suffix = names(file_formats("zips"))){
  pattern <- paste0("\\.", suffix, "$", collapse="|")
  str_detect(file, pattern)
}

file_is_url <- function(file){
  str_detect(file, "^((http|ftp)s?|sftp)://")
}
