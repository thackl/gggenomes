#' Flip bins and sequences
#'
#' `flip` and `flip_seqs` reverse-complement specified bins or individual
#' sequences and their features. `flip_by_links` automatically flips bins using a
#' heuristic that maximizes the amount of forward strand links between
#' neighboring bins.
#'
#' @param x a gggenomes object
#' @param ... bins or sequences to flip in dplyr::select like syntax (numeric
#'   position or unquoted expressions)
#' @param .bins preselection of bins with sequences to flip. Useful if selecting
#'   by numeric position. It sets the context for selection, for example the
#'   11th sequences of the total set might more easily described as the 2nd
#'   sequences of the 3rd bin: `flip_seqs(2, .bins=3)`.
#' @param .bin_track,.seq_track when using a function as selector such as
#'   [tidyselect::where()], this specifies the track in which context the
#'   function is evaluated.
#'
#' @examples
#' library(patchwork)
#' p <- gggenomes(genes=emale_genes) +
#'   geom_seq(aes(color=strand), arrow=TRUE) +
#'   geom_link(aes(fill=strand)) +
#'   expand_limits(color=c("-")) +
#'   labs(caption="not flipped")
#'
#' # nothing flipped
#' p0 <- p %>% add_links(emale_ava)
#'
#' # flip manually
#' p1 <- p %>% add_links(emale_ava) %>%
#'   flip(4:6) + labs(caption="manually")
#'
#' # flip automatically based on genome-genome links
#' p2 <- p %>% add_links(emale_ava) %>%
#'   flip_by_links() + labs(caption="genome alignments")
#'
#' # flip automatically based on protein-protein links
#' p3 <- p %>% add_sublinks(emale_prot_ava) %>%
#'   flip_by_links() + labs(caption="protein alignments")
#'
#' # flip automatically based on genes linked implicitly by belonging
#' # to the same clusters of orthologs (or any grouping of your choice)
#' p4 <- p %>% add_clusters(emale_cogs) %>%
#'   flip_by_links() + labs(caption="shared orthologs")
#'
#' p0 + p1 + p2 + p3 + p4 + plot_layout(nrow=1, guides="collect")
#'
#' # flip seqs inside bins
#' s0 <- tibble(
#'   bin_id = c("A", "B", "B", "B", "C", "C", "C"),
#'   seq_id = c("a1","b1","b2","b3","c1","c2","c3"),
#'   length = c(1e4, 6e3, 2e3, 1e3, 3e3, 3e3, 3e3))
#'
#' p <- gggenomes(seqs=s0) +
#'   geom_seq(aes(color=bin_id), size=1, arrow = arrow(angle = 30, length = unit(10, "pt"),
#'     ends = "last", type = "open")) +
#'   geom_bin_label() + geom_seq_label() +
#'   expand_limits(color=c("A","B","C"))
#'
#' p1 <- p %>% flip_seqs(6)
#' p2 <- p %>% flip_seqs(c2)
#' p3 <- p %>% flip_seqs(2, .bins = C)
#'
#' p + p1 + p2 + p3 + plot_layout(nrow=1, guides="collect")
#'
#' # fancy flipping using tidyselect::where for dynamic selection
#' p <- gggenomes(emale_genes,emale_seqs) %>% add_clusters(emale_cogs) +
#'   geom_seq(color="grey70", size=1, arrow = arrow(angle = 30, length = unit(15, "pt"),
#'     ends = "last", type = "open")) +
#'   geom_gene(aes(fill=cluster_id))
#'
#' # flip all short seqs - where() applied to .bin_track=seqs
#' p1 <- p %>% flip(where(~.x$length < 21000))
#'
#' # flip all seqs with MCP on "-" - where() applied to .bin_track=genes
#' p2 <- p %>% flip(where(~any(.x$strand[.x$cluster_id %in% "cog-MCP"] == "-")), .bin_track=genes)
#'
#' p + p1 + p2 + plot_layout(nrow=1, guides="collect") & theme(legend.position = "bottom")
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
flip_by_links <- function(x, link_track=1, min_coverage=.2){
  UseMethod("flip_by_links")
}
#' @export
flip_by_links.gggenomes <- function(x, link_track=1, min_coverage=.2){
    x$data <- flip_by_links(x$data, link_track={{link_track}}, min_coverage=min_coverage)
    x
}
#' @export
flip_by_links.gggenomes_layout <- function(x, link_track=1, min_coverage=.2){
 if(length(x$links) < 1)
   rlang::abort("Links are required to `flip_by_links`")
  l0 <- pull_links(x, {{link_track}})
  s0 <- ungroup(pull_seqs(x))

  l1 <- l0 %>% group_by(seq_id, seq_id2, strand) %>%
    # treat a-b and b-a the same (lexical order)
    swap_if(seq_id > seq_id2, seq_id, seq_id2) %>%
    summarize(mapped=min(c(sum(width(start,end)), sum(width(start2, end2)))))

  l2 <- l1 %>%
    left_join(by="seq_id", transmute(
      s0, y=y, seq_id=seq_id, seq_width = width(start, end), seq_strand=strand)) %>%
    left_join(by="seq_id2", transmute(
      s0, y2=y, seq_id2=seq_id, seq_width2 = width(start, end), seq_strand2=strand)) %>%
    mutate(coverage = mapped/pmin(seq_width, seq_width2)) %>% group_by(seq_id, seq_id2) %>%
    arrange(-coverage, .by_group = TRUE) %>% summarize_all(first) %>%
    ungroup() %>% select(-seq_width, -seq_width2) %>%
    swap_if(y > y2, seq_id, seq_id2) %>%
    swap_if(y > y2, y, y2) %>% arrange(y)

  l3 <- l2 %>%
    filter(coverage > min_coverage) %>%
    group_by(group=y2 - row_number()) %>%
    mutate(flip=cumprod(strand_int(combine_strands(seq_strand, seq_strand2, strand))))

  y_flip <- l3 %>% filter(flip < 0) %>% pull(y2)

  if(!length(y_flip)){
    inform(str_glue("All bins appear to be flipped nicely based on the given",
                    "links. Maybe change `min_coverage` or flip manually"))
    return(x)
  }else{
    inform(paste("Flipping:", comma(y_flip)))
  }

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
    seq_sel_lst <- seq_sel_lst[names(seq_sel_lst) %in% flip_tbl$seq_id]
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
