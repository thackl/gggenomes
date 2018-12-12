#' @export
as_links <- function(x, ...){
    UseMethod("as_tbl_link")
}

#' @export
as_links.default <- function(x, ...){
    # try to coerce into tbl
    as_tbl_link(as_tibble(x), ...)
}

#' @export
as_links.tbl_df <- function(x, contig_layout, q_cid=1, q_start=2, q_end=3, t_cid=4, t_start=5, t_end=6, strand=7, ..., everything=TRUE){
  
  if(is.null(strand)) x$strand <- 0
   
  x <- if(everything){
    select(x, q_cid=q_cid,q_start=q_start,q_end=q_end,t_cid=t_cid,t_start=t_start,
           t_end=t_end, strand=strand, ..., everything())
  }else{
    select(x, q_cid=q_cid,q_start=q_start,q_end=q_end,t_cid=t_cid,t_start=t_start,
           t_end=t_end, strand=strand, ...)
  }

  x %<>% mutate_if(is.factor, as.character)
  
  if(is_integer(x$strand) || is_double(x$strand)){
    # do nothing
  }else if(rlang::is_character(x$strand)){
    x$strand <- match(x$strand, c("-", "*", "+")) - 2
  }else{
    stop("Unknown strand encoding")
  }

  class(x) <- c("tbl_link", class(x))
  layout(x, contig_layout)
}

# recompute layout
#' @export
layout.tbl_link_layout <- function(x, ...){
    stop("TODO")
}

# backtransform (drop for links, links, backtrans for links
#' @export
as_tbl_link.tbl_link_layout <- function(x, ...){
    stop("TODO")
}

# compute layout
#' @export
as_tbl_link_layout.tbl_link <- function(x, chromosome_layout){
    layout <- chromosome_layout %>% ungroup() %>%
      select(cid, gid, y, offset, gcstrand=strand) %>%
      inner_join(x, .) %>%
      mutate(
        strand = strand*gcstrand,
        x =    dplyr::if_else(strand < 0, offset+end, offset+start),
        xend = dplyr::if_else(strand < 0, offset+start, offset+end)
      ) %>%
      select(y, x, xend, everything())

    class(layout)[class(layout) == 'tbl_link'] <- 'tbl_link_layout'

    layout
}
