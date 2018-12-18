#' Coerce data into a link tibble
#'
#' @inheritParams as_features
#' @export
as_links <- function(x, contigs, ..., everything=TRUE){
    UseMethod("as_links")
}

#' @export
as_links.default <- function(x, contigs, ..., everything=TRUE){
    # try to coerce into tbl
    as_links(as_tibble(x), contigs, ..., everything=everything)
}

#' @export
as_links.tbl_df <- function(x, contigs, ..., everything=TRUE){
  if(!inherits(contigs, "tbl_contig")) stop("expecting contigs as tbl_contig")

  vars <- c("query_contig_id", "target_contig_id")
  require_genome_id <- attr(contigs, "require_genome_id")
  if(!is.null(require_genome_id) && require_genome_id)
    vars <- c(vars, "query_genome_id", "target_genome_id")
  require_vars(x, vars)
  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- as_tibble(select(x, vars, other_vars()))

  TODO("mutate_at - if at all")
  x %<>% mutate_if(is.factor, as.character)
  if(!has_vars(x, "strand")){
    x$strand <- 0L
  }else{
    x$strand <- as_numeric_strand(x$strand)
  }

  layout(set_class(x, "tbl_link", "prepend"), contigs, ...)
}

#' @export
as_tibble.tbl_link <- function(x, ...){
  stop("laying out links is a lossy process and cannot be reversed!")
}

as_feature_links <- function(x, features, ..., everything=TRUE){
  if(!inherits(contigs, "tbl_contig")) stop("expecting contigs as tbl_contig")
  if(!inherits(features, "tbl_feature")) stop("expecting featuress as tbl_feature")
}

#' Layout tbl_link
#'
#' @inheritParams as_links
#' @param ... not used
layout.tbl_link <- function(x, contigs, features=NULL, adjacent_only=TRUE, ...){
  contigs %<>% ungroup

  if(!has_vars(x, c("query_start", "query_end", "target_start", "target_end"))){
    if(has_vars(x, c("query_start", "query_end", "target_start", "target_end"),
                any=TRUE)){ stop("Need either all of query_start,query_end,target_start,target_end or none!")}
    query_contigs <- select(contigs, query_contig_id=contig_id, q.gix=.gix,
           query_end=length) %>% mutate(query_start=0)
    target_contigs <- select(contigs, target_contig_id=contig_id, t.gix=.gix,
        target_end=length) %>% mutate(target_start=0)
  }else{
    query_contigs <- select(contigs, query_contig_id=contig_id, q.gix=.gix)
    target_contigs <- select(contigs, target_contig_id=contig_id, t.gix=.gix)
  }
  x %<>% inner_join(query_contigs) %>% inner_join(target_contigs)

  # adjacent links
  if(adjacent_only) # remove links between non-adjacent contig_layout
    x %<>% filter(abs(t.gix-q.gix)==1)

  if(nrow(x)==0){
    stop("No links found between adjacent genomes in provided contig_layout, consider reordering genomes")
  }

  x %<>% mutate_if(is.factor, as.character)
  if(has_name(x, "strand")){
    x$strand <- 0L
  }else{
    x$strand <- as_numeric_strand(x$strand)
  }

  x %<>% mutate(.lix=row_number()) %>%
    # polygon-id - order of points in polygon
    gather(".pid", "x", query_start, query_end, target_end, target_start) %>%
    mutate(contig_id=ifelse(.pid %in% c("query_start", "query_end"), query_contig_id, target_contig_id)) %>%
    select(-starts_with("q."), -starts_with("t.")) %>%
    arrange(.lix) %>%
    inner_join(select(contigs, genome_id, contig_id, .gix, .offset)) %>%
    mutate(x=x+.offset) %>%
    arrange(.lix, .gix)

  x$.pix <- rep(1:4, nrow(x)/4)
  x$.pix[x$.pix==3 & x$strand < 0] <- 5
  x$.nudge_sign <- rep(c(1,1,-1,-1), nrow(x)/4)

  set_class(x, "tbl_link", "prepend")
}
