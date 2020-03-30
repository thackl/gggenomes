#' Compute a genomes layout from sequences, features and links
#'
#' See `gggenomes::gggenomes()` for more info.
#' @rdname as_genomes
#' @param seqs a table with sequence data (seq_id, bin_id, length)
#' @param features a table (or names list of tables) with feature data (seq_id,
#' bin_id, start, end)
#' @param links a table with link data (from, to, from_start, from_end,
#' to_start, to_end)
#' @param ... layout parameters passed on to `layout_seqs()`
#' @export
layout_genomes <- function(seqs=NULL, features=NULL, links=NULL, .feature_id = "genes", .link_id = "links", ...){

  x <- list(seqs = NULL, features = list(), links = list(), orig_links = list(),
            params = list())
  x %<>% set_class("gggenomes_layout", "prepend")

  if(!is.null(features) & !is.list(features))
    features <- list(.feature_id = features)
  if(!is.null(links) & !is.list(links))
    links <- list(.link_id = features)

  if(is.null(seqs)){
    if(is.null(features) & is.null(links))
      stop("Need at least one of: contigs, genes or links")
    else
      write("No seqs provided, inferring seqs from features/links", stderr())

    # generate dummy contigs
    if(!is.null(features))
      seqs <- infer_seqs_from_features(features) 
    else if(!is.null(links))
      seqs <- infer_seqs_from_links(links)
  }

  x %<>% add_seqs(seqs, ...) # layout seqs
  #if(!is.null(features)) x <- do.call(add_features, features)
  #if(!is.null(links)) x <- do.call(add_links, links)
  x
}

#' `ggplot2::facet_null` checks data with `empty(df)` using `dim`. This causes
#' and error because dim(gggenome_layout) is undefined. Return dim of primary
#' table instead
#' @export
dim.gggenomes_layout <- function(x) dim(x$seqs)


infer_seqs_from_features <- function(features){
  contigs <- genes %>%
    mutate(genome_id = str_replace(contig_id, "_?\\w?\\d+$", "")) %>%
    group_by(genome_id, contig_id) %>%
    summarize(length = max(end))
  
}

infer_seqs_from_links <- function(links){
  contigs <- bind_rows(
    select_at(links, vars(starts_with("query")), str_replace, "query_", ""),
    select_at(links, vars(starts_with("target")), str_replace, "target_", "")
  ) %>%
    mutate(genome_id = str_replace(contig_id, "_?\\w?\\d+$", "")) %>%
    group_by(genome_id, contig_id) %>%
    summarize(length = max(end))
}

