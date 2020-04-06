#' Compute a layout for link data
#'
#' Read link data of pairwise sequence or feature comparisons, such as
#' similarity searches into a tidy dataframe and augment it with layout
#' information based on a sequence layout.
#'
#' Obligatory columns are `from_id`, `to_id`. Also recognized are `from_start`,
#' `from_end`,`to_start`,`to_end`,`strand` and `bin_id`.
#'
#' @param x link data convertible to a link layout
#' @inheritParams as_features
#' @export
as_links <- function(x, seqs, ..., everything=TRUE){
    UseMethod("as_links")
}

#' @export
as_links.default <- function(x, seqs, ..., everything=TRUE){
    # try to coerce into tbl
    as_links(as_tibble(x), seqs, ..., everything=everything)
}

#' @export
as_links.tbl_df <- function(x, seqs, ..., everything=TRUE){
  vars <- c("from_id", "to_id")
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

  layout_links(x, seqs, ...)
}

#' @export
as_tibble.tbl_link <- function(x, ...){
  stop("laying out links is a lossy process and cannot be reversed!")
}

#' Layout tbl_link
#'
#' @inheritParams as_links
#' @param ... not used
layout_links <- function(x, seqs, features=NULL, adjacent_only=TRUE, ...){
  seqs %<>% ungroup

  if(!has_vars(x, c("from_start", "from_end", "to_start", "to_end"))){
    if(has_vars(x, c("from_start", "from_end", "to_start", "to_end"),
                any=TRUE)){
      stop("Need either all of from_start,from_end,to_start,to_end or none!")}
    from_seqs <- select(seqs, from_id=seq_id, from_y=y,
      from_end=length, from_strand=strand) %>% mutate(from_start=0)
    to_seqs <- select(seqs, to_id=seq_id, to_y=y,
      to_end=length, to_strand=strand) %>% mutate(to_start=0)
  }else{
    from_seqs <- select(seqs, from_id=seq_id, from_y=y,
                            from_strand=strand)
    to_seqs <- select(seqs, to_id=seq_id, to_y=y,
                             to_strand=strand)
  }
  x %<>% inner_join(from_seqs) %>% inner_join(to_seqs)

  # adjacent links
  if(adjacent_only) # remove links between non-adjacent seq_layout
    x %<>% filter(abs(to_y-from_y)==1)

  if(nrow(x)==0){
    warning("No links found between adjacent genomes in provided seq_layout, consider reordering genomes")
    return(tibble())
  }

  x %<>% mutate(.lix=row_number()) %>%
    # polygon-id - order of points in polygon
    gather(".pid", "x", from_start, from_end, to_end, to_start) %>%
    mutate(
      seq_id=ifelse(.pid %in% c("from_start", "from_end"), from_id, to_id)) %>%
    select(-from_y, -from_strand, -to_y, -to_strand) %>%
    arrange(.lix) %>%
    inner_join(transmute(seqs, seq_id, y, .seq_length=length, # bin_id included
       .seq_strand=strand, .seq_offset=pmin(x,xend))) %>%     # b/c group var
    mutate(x=.seq_offset+ifelse(.seq_strand >= 0, x, (x-.seq_length)*-1)) %>%
    group_by(.lix) %>%
    mutate(.x_center = mean(x), .y_center = mean(y)) %>% ungroup %>%
    arrange(.lix, y)

  # index polygon points
  x$.pix <- rep(1:4, nrow(x)/4)
  x$.pix[x$.pix==3 & x$strand < 0] <- 5
  x$.nudge_sign <- rep(c(1,1,-1,-1), nrow(x)/4)
  x %<>% arrange(.lix, y, .pix)
}
