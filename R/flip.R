#' Flip bins and sequences
#'
#' \code{flip} and \code{flip_seqs} reverse-complement specified bins or
#' individual sequences and their feats. \code{flip_nicely} automatically
#' flips bins using a heuristic that maximizes the amount of forward strand
#' links between neighboring bins.
#' @param x a gggenomes object
#' @param ... bins or sequences to flip in dplyr::select like syntax (numeric
#'     position or unquoted expressions)
#' @param .bins preselection of bins with sequences to flip. Useful if selecting
#'     by numeric position. It sets the context for selection, for example the
#'     11th sequences of the total set might more easily described as the 2nd
#'     sequences of the 3rd bin: \code{flip_seqs(2, .bins=3)}
#' @param .bin_track,.seq_track when using a function as selector such as
#'     \link{\code{tidyselect::where}}, this specifies the track in which
#'     context the function is evaluated.
#' @examples
#' p <- gggenomes(emale_seqs[1:7,], links=emale_links) +
#'   geom_seq(aes(color=strand), arrow=TRUE) + geom_link() +
#'   scale_color_manual(values=c("+"="blue", "-"="red"))
#' # some bins would align nicer if reverse-complemented
#' p
#'
#' # flip bins 3, 4 and 5
#' p %>% flip(3:5)
#'
#' # flip automatically based on links
#' p %>% flip_nicely()
#'
#' # also works with links from clusters
#' p <- gggenomes(emale_seqs[1:7,], emale_genes) +
#'   geom_seq(aes(color=strand), arrow=TRUE) + geom_link() +
#'   scale_color_manual(values=c("+"="blue", "-"="red"))
#' p %>%
#'   add_clusters(genes, emale_cogs) %>%
#'   flip_nicely(emale_cogs)
#' @export
flip <- function(x, ..., .bin_track=seqs){
  UseMethod("flip")
}
#' @export
flip.gggenomes <- function(x, ..., .bin_track=seqs){
  x$data <- flip(x$data, ..., .bin_track={{ .bin_track }})
  x
}
#' @export
flip.gggenomes_layout <- function(x, ..., .bin_track=seqs){
  if(!has_dots()) return(x)
  flip_impl(x, bins=c(...), bin_track={{.bin_track}})
}
#' @rdname flip
#' @export
flip_seqs <- function(x, ..., .bins=everything(), .seq_track=seqs, .bin_track=seqs){
  UseMethod("flip_seqs")
}
#' @export
flip_seqs.gggenomes <- function(x, ..., .bins=everything(), .seq_track=seqs, .bin_track=seqs){
  x$data <- flip_seqs(x$data, ..., .bins={{.bins}}, .bin_track={{.bin_track}},
      .seq_track={{.seq_track}})
  x
}
#' @export
flip_seqs.gggenomes_layout <- function(x, ..., .bins=everything(), .seq_track=seqs, .bin_track=seqs){
  if(!has_dots()) return(x)
  flip_impl(x, bins={{.bins}}, seqs=c(...), bin_track={{.bin_track}}, seq_track={{.seq_track}})
}
#' @rdname flip
#' @param link_track the link track to use for flipping bins nicely
#' @param min_coverage at least this much of the shorter bin must be covered by
#'   links supporting a flip to actually carry it out.
#' @export
flip_nicely <- function(x, link_track=1, min_coverage=.2){
  UseMethod("flip_nicely")
}
#' @export
flip_nicely.gggenomes <- function(x, link_track=1, min_coverage=.2){
    x$data <- flip_nicely(x$data, link_track={{link_track}}, min_coverage=min_coverage)
    x
}
#' @export
flip_nicely.gggenomes_layout <- function(x, link_track=1, min_coverage=.2){
 if(length(x$links) < 1)
   rlang::abort("Links are required to flip bins nicely")
  l0 <- pull_links(x, {{link_track}})
  s0 <- ungroup(pull_seqs(x))

  l1 <- l0 %>% group_by(seq_id1, seq_id2, strand) %>%
    summarize(mapped=min(c(sum(width(start1,end1)), sum(width(start2, end2)))))
  l1

  l2 <- l1 %>%
    left_join(by="seq_id1", transmute(
      s0, y1=y, seq_id1=seq_id, seq_width1 = width(start, end), seq_strand1=strand)) %>%
    left_join(by="seq_id2", transmute(
      s0, y2=y, seq_id2=seq_id, seq_width2 = width(start, end), seq_strand2=strand)) %>%
    mutate(coverage = mapped/pmin(seq_width1, seq_width2)) %>% group_by(seq_id1, seq_id2) %>%
    arrange(-coverage, .by_group = TRUE) %>% summarize_all(first) %>%
    ungroup() %>% select(-seq_width1, -seq_width2) %>%
    swap_if(y1 > y2, seq_id1, seq_id2) %>%
    swap_if(y1 > y2, y1, y2) %>% arrange(y1)

  l3 <- l2 %>%
    filter(coverage > min_coverage) %>%
    group_by(group=y2 - row_number()) %>%
    mutate(flip=cumprod(strand_int(combine_strands(seq_strand1, seq_strand2, strand))))

  y_flip <- l3 %>% filter(flip < 0) %>% pull(y2)

  x %>% flip(all_of(y_flip))
}

flip_impl <- function(x, bins=everything(), seqs=NULL, bin_track=seqs, seq_track=seqs){
  # split by bin_id and select bins
  seq_tbl <- pull_seqs(x)
  seq_lst <- split_by(seq_tbl, bin_id)
  # in case we want to compute selections based on a track other than seqs
  bin_sel_lst <- split_by(pull_track(x, {{bin_track}}), bin_id)
  bin_i <- tidyselect::eval_select(expr({{ bins }}), bin_sel_lst)
  if(length(bin_i) == 0) rlang::abort("no bins selected")
  # select bins to operate on
  flip_tbl <- bind_rows(seq_lst[names(bin_i)])
    
  # flip seqs in bins
  seqs <- enquo(seqs)
  if(!quo_is_null(seqs)){
    seq_sel_lst <- split_by(pull_track(x, {{seq_track}}), seq_id)
    seq_i <- tidyselect::eval_select(expr(!! seqs ), seq_sel_lst)
    seq_i <- flip_tbl$seq_id %in% names(seq_i)
    flip_tbl$strand[seq_i] <- flip_strand(flip_tbl$strand[seq_i])
  # flip entire bins
  }else{
    flip_tbl %<>% group_by(bin_id) %>%
      mutate(strand = flip_strand(strand)) %>%
      arrange(-row_number(), .by_group=TRUE)
  }

  # splice modified bins back into rest
  flip_lst <- flip_tbl %>% thacklr::split_by(bin_id)
  seq_lst[names(flip_lst)] <- flip_lst
  seq_tbl <- bind_rows(seq_lst)

  x <- set_seqs(x, seq_tbl)
  layout(x)
}

