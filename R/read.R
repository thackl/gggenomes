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

#' Read AliTV .json file
#'
#' this file contains sequences, links and (optionally) genes
#'
#' @importFrom tidyr unnest_wider
#' @importFrom tidyr unnest
#' @importFrom jsonlite fromJSON
#' @param file path to json
#' @export
#' @return list with seqs, genes, and links
#' @examples
#' ali <- read_alitv("https://alitvteam.github.io/AliTV/d3/data/chloroplasts.json")
#' gggenomes(ali$seqs, ali$genes, links=ali$links) +
#'   geom_seq() +
#'   geom_bin_label() +
#'   geom_gene(aes(fill=class)) +
#'   geom_link()
#' p <- gggenomes(ali$seqs, ali$genes, links=ali$links) +
#'   geom_seq() +
#'   geom_bin_label() +
#'   geom_gene(aes(color=class)) +
#'   geom_link(aes(fill=identity)) +
#'   scale_fill_distiller(palette="RdYlGn", direction = 1)
#' p %>% flip_seq("Same_gi") %>% pick(1,3,2,4,5,6,7,8)
read_alitv <- function(file){
  ali <- jsonlite::fromJSON(file, simplifyDataFrame=TRUE)
  seqs <- tibble(seq = ali$data$karyo$chromosome) %>%
    mutate(seq_id = names(seq)) %>%
    unnest_wider(seq) %>%
    rename(bin_id = genome_id)
  genes <- tibble(feature = ali$data$feature) %>%
    mutate(class = names(feature)) %>%
    filter(class != "link") %>%
    unnest(feature) %>%
    rename(seq_id=karyo)
  links <- tibble(links=ali$data$links) %>% unnest(links) %>% unnest(links) %>% unnest_wider(links)
  link_pos <- tibble(link=ali$data$features$link) %>% mutate(id=names(link)) %>% unnest_wider(link)
  links <- links %>%
    left_join(link_pos, by=c("source"="id")) %>%
    left_join(link_pos, by=c("target"="id")) %>%
    transmute(
        seq_id1=karyo.x,
        start1=start.x,
        end1=end.x,
        seq_id2=karyo.y,
        start2=start.y,
        end2=end.y,
        identity=identity
    )
  return(list(seqs=seqs,genes=genes,links=links))
}
