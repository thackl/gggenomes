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
    as_tbl_link(as_tibble(x), ...)
}

#' @export
as_links.tbl_df <- function(x, contigs, ..., everything=TRUE){
  if(!inherits(contigs, "tbl_contig")) stop("expecting contigs as tbl_contig")

  vars <- c("query_contig_id", "query_start", "query_end",
            "target_contig_id", "target_start","target_end")
  require_genome_id <- attr(contigs, "require_genome_id")
  if(!is.null(require_genome_id) && require_genome_id)
    vars <- c(vars, "query_genome_id", "target_genome_id")
  require_vars(x, vars)
  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- as_tibble(select(x, vars, other_vars()))

  TODO("mutate_at - if at all")
  x %<>% mutate_if(is.factor, as.character)
  if(!has_vars(x, "strand")){
    x$.strand <- 0L
  }else{
    x$.strand <- as_numeric_strand(x$strand)
  }

  layout(set_class(x, "tbl_link", "prepend"), contigs, ...)
}

# recompute layout
#' @export
layout.tbl_link <- function(x, ...){
    stop("TODO")
}

#' Layout tbl_link
#'
#' @inheritParams as_links
#' @param ... not used
layout.tbl_link <- function(x, contigs, ...){
  .x <- x
  x %<>%
    inner_join(contigs %>% ungroup %>% select(query_contig_id=contig_id, q.gix=.gix)) %>%
    inner_join(contigs %>% ungroup %>% select(target_contig_id=contig_id, t.gix=.gix)) %>%
    filter(abs(t.gix-q.gix)==1) # filter links between non-adjacent contig_layout
  
  if(nrow(x)==0){
    stop("No links found between adjacent genomes in provided contig_layout, consider reordering genomes")
  }

  x %<>% mutate_if(is.factor, as.character)
  if(!has_vars(x, "strand")){
    x$.strand <- 0L
  }else{
    x$.strand <- as_numeric_strand(x$strand)
  }
  
  x %<>% mutate(.lix=row_number()) %>%
    gather(".pid", "x", query_start, query_end, target_start, target_end) %>%
    mutate(contig_id=ifelse(.pid %in% c("query_start", "query_end"), query_contig_id, target_contig_id)) %>%
    select(-starts_with("q."), -starts_with("t.")) %>%
    arrange(.lix) %>%
    inner_join(contigs %>% ungroup %>% select(genome_id, contig_id, .gix, .offset)) %>%
    mutate(x=x+.offset) %>%
    arrange(.lix, .gix) 

    x$.pix <- rep(1:4, nrow(x)/4)
    x$.pix[x$.pix==3 & x$.strand < 0] <- 5
    x$.nudge_sign <- rep(c(1,1,-1,-1), nrow(x)/4)

  list(x, .x) # return tbl_link and orig links
}


as_feature_links <- function(){

}
