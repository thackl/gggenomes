#' Flip bins and sequences
#'
#' \code{flip} and \code{flip_seq} reverse-complement specified bins or
#' individual sequences and their features. \code{flip_nicely} automatically
#' flips bins using a heuristic that maximizes the amount of forward strand
#' links between neighboring bins.
#' @param x a gggenomes object
#' @param ... bins or sequences to flip in dplyr::select like syntax (numeric
#'   position or unquoted expressions)
#' @param .bins preselection of bins with sequences to flip. Useful if selecting
#'   by numeric position. It sets the context for selection, for example the
#'   11th sequences of the total set might more easily described as the 2nd
#'   sequences of the 3rd bin: \code{flip_seq(2, bins=3)}
#' @export
flip <- function(x, ...){
  UseMethod("flip")
}
#' @export
flip.gggenomes <- function(x, ...){
  x$data <- flip(x$data, ...)
  x
}
#' @export
flip.gggenomes_layout <- function(x, ...){
  if(!has_dots()) return(x)
  flip_impl(x, .bins=c(...))
}
#' @rdname flip
#' @export
flip_seq <- function(x, ..., .bins=everything()){
  UseMethod("flip_seq")
}
#' @export
flip_seq.gggenomes <- function(x, ..., .bins=everything()){
  x$data <- flip_seq(x$data, ..., .bins = {{ .bins }})
  x
}
#' @export
flip_seq.gggenomes_layout <- function(x, ..., .bins=everything()){
  if(!has_dots()) return(x)
  flip_impl(x, ..., .bins = {{ .bins }})
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

flip_impl <- function(x, ..., .bins=everything()){
  # split by bin_id and select bins
  s <- seqs(x)
  l <- s %>% thacklr::split_by(bin_id)
  i <- tidyselect::eval_select(expr({{ .bins }}), l)
  if(length(i) == 0) rlang::abort("no bins selected")
  s <- bind_rows(l[i])


  # flip seqs in bins
  if(length(ellipsis:::dots()) > 0){
    seq_ids <- s$seq_id %>% set_names(.)
    j <- tidyselect::eval_select(expr(c(...)), seq_ids)
    s$strand[j] <- flip_strand(s$strand[j])
  # flip entire bins
  }else{
    s %<>% mutate(strand = flip_strand(strand)) %>%
      arrange(-row_number())
  }

  # splice modified bins back into rest
  m <- s %>% thacklr::split_by(bin_id)
  l[names(m)] <- m
  s <- bind_rows(l)

  seqs(x) <- s
  layout(x)
}

