#' Compute a layout for link data
#'
#' Read link data of pairwise sequence or feature comparisons, such as
#' similarity searches into a tidy dataframe and augment it with layout
#' information based on a sequence layout.
#'
#' Obligatory columns are `seq_id1` and `seq_id2`. Also recognized are
#' `start1`, `end1`,`start2`,`end2`,`strand`, bin_id1` and `bin_id2.
#'
#' During layouting, seq_id1,start1,end1 will be projected to x,xend,y, while
#' seq_id2,start2,end2 will be projected to xmin,xmax,yend. gggenomes uses these
#' maybe a bit odd names for the variables here, is so that they play nice with
#' ggplots native transformation functions for position aesthetics. Those only
#' work well with a specific set of predefined var names, which include those
#' used above.
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
  vars <- c("seq_id1", "seq_id2")
  require_vars(x, vars)

  # coerce IDs to chars, so we don't get errors in join by mismatched types
  x <- mutate_at(x, vars(seq_id1, seq_id2), as.character)

  if(!has_vars(x, c("start1", "end1", "start2", "end2"))){
    if(has_vars(x, c("start1", "end1", "start2", "end2"),any=TRUE)){
      abort("Need either all of start1,fend1,start2,end2 or none!")
    }

    x <- left_join(select(seqs, seq_id1=seq_id, start1=start, end1 = end)) %>%
      left_join(select(seqs, seq_id2=seq_id, start2=start, end2 = end))
  }
  vars <- c("seq_id1", "start1", "end1", "seq_id2", "start2", "end2")

  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- as_tibble(select(x, vars, other_vars()))

  TODO("mutate_at - if at all")
  x %<>% mutate_if(is.factor, as.character)
  if(!has_vars(x, "strand")){
    x$strand <- "+"
  }else{
    x$strand <- strand_chr(x$strand)
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
layout_links <- function(x, seqs, keep="strand", adjacent_only = TRUE,
  marginal=c("trim", "drop", "keep"), ...){
  marginal <- match.arg(marginal)
  # get rid of old layout
  x <- drop_link_layout(x, keep)

  # get layout vars necessary for projecting features from seqs
  x <- add_link_layout_scaffold(x, seqs)

  # TODO: adjacent_only can currently not overwritten by add_links() this is
  # mostly for performance - no need to compute on a-v-a links, of only adjacent
  # ones are displayed in the end
  if(adjacent_only){
    x <- filter(x, abs(y-yend) == 1)
    if(nrow(x)==0){
      warning("No links found between adjacent genomes in provided order of genomes, consider reordering genomes")
      return(tibble())
    }
  }

  # ignore features outside subseqs
  x <- trim_links_to_subseqs(x, marginal)

  # project features onto new layout and clean up aux vars (.seq)
  x <- project_links(x) %>%
    select(y, x, xend, yend, xmin, xmax, everything(), -starts_with(".seq"))
  x
}


#' @export
add_links <- function(x, ...){
  UseMethod("add_links")
}

#' @export
add_links.gggenomes <- function(x, ...){
  x$data <- add_links(x$data, ...)
  x
}

#' @export
add_links.gggenomes_layout <- function(x, ..., .auto_prefix="links"){
  tracks <- list(...)
  names(tracks) <- check_track_ids(names(tracks), track_ids(x), "links",
                                      .auto_prefix)
  # convert to layouts
  x$links <- c(x$links, map(tracks, as_links, x$seqs)) # this is lossy, so
  x$orig_links <- c(x$orig_links, tracks) # also store orig links for re-layout
  x
}


#' @export
drop_link_layout <- function(x, seqs, keep="strand"){
  drop <- c("y","x","xend","yend","xmin","xmax","strand", grep("^\\.", names(x), value=T))
  drop <- drop[!drop %in% keep]
  discard(x, names(x) %in% drop)
}

add_link_layout_scaffold <- function(x, seqs){
  scaffold1 <- seqs %>% ungroup() %>% select(
    seq_id1=seq_id, bin_id1=bin_id, y=y, .seq_strand1=strand, .seq_x1=x,
    .seq_start1=start, .seq_end1=end)
  scaffold2 <- seqs %>% ungroup() %>% select(
    seq_id2=seq_id, bin_id2=bin_id, yend=y, .seq_strand2=strand, .seq_x2=x,
    .seq_start2=start, .seq_end2=end)

  join_by <- if(has_name(x, "bin_id1")){c("seq_id1", "bin_id1")}else{"seq_id1"}
  x <- inner_join(x, scaffold1, by=join_by)
  join_by <- if(has_name(x, "bin_id2")){c("seq_id2", "bin_id2")}else{"seq_id2"}
  inner_join(x, scaffold2, by=join_by)
}

trim_links_to_subseqs <- function(x, marginal){
  if(marginal == "drop"){
    x <- mutate(x, .marginal1 = FALSE, .marginal2 = FALSE)
  }else{
    x <- mutate(x,
      .marginal1 = is_marginal(start1, end1, .seq_start1, .seq_end1),
      .marginal2 = is_marginal(start2, end2, .seq_start2, .seq_end2))
  }

  if(marginal == "trim"){
    x %<>% mutate(
      start1 = ifelse(.marginal1 & start1 < .seq_start1, .seq_start1, start1),
      end1 = ifelse(.marginal1 & end1 > .seq_end1, .seq_end1, end1),
      start2 = ifelse(.marginal2 & start2 < .seq_start2, .seq_start2, start2),
      end2 = ifelse(.marginal2 & end2 > .seq_end2, .seq_end2, end2))
  } # marginals are now also fully contained

  filter(x,
    .seq_start1 <= start1 & end1 <= .seq_end1 | .marginal1,
    .seq_start2 <= start2 & end2 <= .seq_end2 | .marginal2,
  )
}

project_links <- function(x){
  dummy <- rep("+", nrow(x))
  mutate(x,
    x =       x(start1, end1, dummy,  .seq_x1, .seq_start1, .seq_strand1),
    xend = xend(start1, end1, dummy,  .seq_x1, .seq_start1, .seq_strand1),
    xmin =    x(start2, end2, strand, .seq_x2, .seq_start2, .seq_strand2),
    xmax = xend(start2, end2, strand, .seq_x2, .seq_start2, .seq_strand2)
  )
}
