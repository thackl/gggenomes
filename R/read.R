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
