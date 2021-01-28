#' Compute a layout for link data
#'
#' Read link data of pairwise sequence or feat comparisons, such as
#' similarity searches into a tidy dataframe and augment it with layout
#' information based on a sequence layout.
#'
#' Obligatory columns are `seq_id` and `seq_id2`. Also recognized are
#' `start`, `end`,`start2`,`end2`,`strand`, bin_id` and `bin_id2.
#'
#' During layouting, seq_id,start,end will be projected to x,xend,y, while
#' seq_id2,start2,end2 will be projected to xmin,xmax,yend. gggenomes uses these
#' maybe a bit odd names for the variables here, is so that they play nice with
#' ggplots native transformation functions for position aesthetics. Those only
#' work well with a specific set of predefined var names, which include those
#' used above.
#'
#' @param x link data convertible to a link layout
#' @inheritParams as_feats
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
  vars <- c("seq_id", "seq_id2")
  require_vars(x, vars)

  # coerce IDs to chars, so we don't get errors in join by mismatched types
  x <- mutate_at(x, vars(seq_id, seq_id2), as.character)

  if(!has_vars(x, c("start", "end", "start2", "end2"))){
    if(has_vars(x, c("start", "end", "start2", "end2"),any=TRUE)){
      abort("Need either all of start,fend1,start2,end2 or none!")
    }

    x <- x %>%
      left_join(select(ungroup(seqs), seq_id=seq_id, start=start, end = end), by="seq_id") %>%
      left_join(select(ungroup(seqs), seq_id2=seq_id, start2=start, end2 = end), by="seq_id2")
  }
  vars <- c("seq_id", "start", "end", "seq_id2", "start2", "end2")

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

#' Layout tbl_link
#'
#' @inheritParams as_links
#' @param ... not used
layout_links <- function(x, seqs, keep="strand", adjacent_only = TRUE,
  marginal=c("trim", "drop", "keep"), ...){
  marginal <- match.arg(marginal)
  # get rid of old layout
  x <- drop_link_layout(x, keep)

  # get layout vars necessary for projecting feats from seqs
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

  # ignore feats outside subseqs
  x <- trim_links_to_subseqs(x, marginal)

  # project feats onto new layout and clean up aux vars (.seq)
  x <- project_links(x) %>%
    select(y, x, xend, yend, xmin, xmax, everything(), -starts_with(".seq"))
  x
}


#' Add links
#' @param ... link tables, possibly named, i.e. blast=blast_df, domains=domain_df
#' @inheritParams as_links
#' @examples
#' gggenomes(emale_seqs[1:4,]) %>%
#'   add_links(links=emale_links) +
#'   geom_seq() + geom_link()
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
add_links.gggenomes_layout <- function(x, ...){
  if(!has_dots()) return(x)
  dot_exprs <- enexprs(...) # defuse before list(...)
  tracks <- as_tracks(list(...), dot_exprs, track_ids(x))
  # convert to layouts
  add_link_tracks(x, tracks)
}

add_link_tracks <- function(x, tracks){
  x$links <- c(x$links, map(tracks, as_links, get_seqs(x))) # this is lossy, so
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
  scaffold <- seqs %>% ungroup() %>% select(
    seq_id=seq_id, bin_id=bin_id, y=y, .seq_strand=strand, .seq_x=x,
    .seq_start=start, .seq_end=end)
  scaffold2 <- seqs %>% ungroup() %>% select(
    seq_id2=seq_id, bin_id2=bin_id, yend=y, .seq_strand2=strand, .seq_x2=x,
    .seq_start2=start, .seq_end2=end)

  x <- inner_join(x, scaffold, by=shared_names(x, "seq_id", "bin_id"))
  x <- inner_join(x, scaffold2, by=shared_names(x, "seq_id2", "bin_id2"))
  x
}

trim_links_to_subseqs <- function(x, marginal){
  if(marginal == "drop"){
    x <- mutate(x, .marginal = FALSE, .marginal2 = FALSE)
  }else{
    x <- mutate(x,
      .marginal = is_marginal(start, end, .seq_start, .seq_end),
      .marginal2 = is_marginal(start2, end2, .seq_start2, .seq_end2))
  }

  if(marginal == "trim"){
    x %<>% mutate(
      start = ifelse(.marginal & start < .seq_start, .seq_start, start),
      end = ifelse(.marginal & end > .seq_end, .seq_end, end),
      start2 = ifelse(.marginal2 & start2 < .seq_start2, .seq_start2, start2),
      end2 = ifelse(.marginal2 & end2 > .seq_end2, .seq_end2, end2))
  } # marginals are now also fully contained

  filter(x,
    .seq_start <= start & end <= .seq_end | .marginal,
    .seq_start2 <= start2 & end2 <= .seq_end2 | .marginal2,
  )
}

project_links <- function(x){
  dummy <- rep("+", nrow(x))
  mutate(x,
    x =       x(start, end, dummy,  .seq_x, .seq_start, .seq_strand),
    xend = xend(start, end, dummy,  .seq_x, .seq_start, .seq_strand),
    xmin =    x(start2, end2, strand, .seq_x2, .seq_start2, .seq_strand2),
    xmax = xend(start2, end2, strand, .seq_x2, .seq_start2, .seq_strand2)
  )
}
