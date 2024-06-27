#' Flip bins and sequences
#'
#' `flip` and `flip_seqs` reverse-complement specified bins or individual
#' sequences and their features. `sync` automatically flips bins using a
#' heuristic that maximizes the amount of forward strand links between
#' neighboring bins.
#'
#' For more details see the help vignette:
#' \code{vignette("flip", package = "gggenomes")}
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
#' @return a gggenomes object with flipped bins or sequences
#'
#' @examples
#' library(patchwork)
#' p <- gggenomes(genes = emale_genes) +
#'   geom_seq(aes(color = strand), arrow = TRUE) +
#'   geom_link(aes(fill = strand)) +
#'   expand_limits(color = c("-")) +
#'   labs(caption = "not flipped")
#'
#' # nothing flipped
#' p0 <- p %>% add_links(emale_ava)
#'
#' # flip manually
#' p1 <- p %>%
#'   add_links(emale_ava) %>%
#'   flip(4:6) + labs(caption = "manually")
#'
#' # flip automatically based on genome-genome links
#' p2 <- p %>%
#'   add_links(emale_ava) %>%
#'   sync() + labs(caption = "genome alignments")
#'
#' # flip automatically based on protein-protein links
#' p3 <- p %>%
#'   add_sublinks(emale_prot_ava) %>%
#'   sync() + labs(caption = "protein alignments")
#'
#' # flip automatically based on genes linked implicitly by belonging
#' # to the same clusters of orthologs (or any grouping of your choice)
#' p4 <- p %>%
#'   add_clusters(emale_cogs) %>%
#'   sync() + labs(caption = "shared orthologs")
#'
#' p0 + p1 + p2 + p3 + p4 + plot_layout(nrow = 1, guides = "collect")
#' @export
flip <- function(x, ..., .bin_track = seqs) {
  UseMethod("flip")
}
#' @export
flip.gggenomes <- function(x, ..., .bin_track = seqs) {
  x$data <- flip(x$data, ..., .bin_track = {{ .bin_track }})
  x
}
#' @export
flip.gggenomes_layout <- function(x, ..., .bin_track = seqs) {
  if (!has_dots()) {
    return(x)
  }
  flip_impl(x, bins = c(...), bin_track = {{ .bin_track }})
}
#' @rdname flip
#' @export
flip_seqs <- function(x, ..., .bins = everything(), .seq_track = seqs, .bin_track = seqs) {
  UseMethod("flip_seqs")
}
#' @export
flip_seqs.gggenomes <- function(x, ..., .bins = everything(), .seq_track = seqs, .bin_track = seqs) {
  x$data <- flip_seqs(x$data, ...,
    .bins = {{ .bins }}, .bin_track = {{ .bin_track }},
    .seq_track = {{ .seq_track }}
  )
  x
}
#' @export
flip_seqs.gggenomes_layout <- function(x, ..., .bins = everything(), .seq_track = seqs, .bin_track = seqs) {
  if (!has_dots()) {
    return(x)
  }
  flip_impl(x, bins = {{ .bins }}, seqs = c(...), bin_track = {{ .bin_track }}, seq_track = {{ .seq_track }})
}
#' @rdname flip
#' @param link_track the link track to use for flipping bins nicely
#' @param min_support only flip a bin if at least this many more nucleotides
#'   support an inversion over the given orientation
#' @export
sync <- function(x, link_track = 1, min_support = 0) {
  UseMethod("sync")
}
#' @export
sync.gggenomes <- function(x, link_track = 1, min_support = 0) {
  x$data <- sync(x$data, link_track = {{ link_track }}, min_support = min_support)
  x
}
#' @export
sync.gggenomes_layout <- function(x, link_track = 1, min_support = 0) {
  if (length(x$links) < 1) {
    rlang::abort("Links are required to `sync`")
  }
  l0 <- pull_links(x, {{ link_track }})
  s0 <- ungroup(pull_seqs(x))

  f0 <- l0 |>
    dplyr::left_join(select(s0, .data$seq_id, seq_strand = .data$strand), by = "seq_id") |>
    dplyr::left_join(select(s0, seq_id2 = .data$seq_id, seq_strand2 = .data$strand), by = "seq_id2") |>
    dplyr::mutate(
      bin_id = ifelse(.data$y < .data$yend, .data$bin_id, .data$bin_id2), # chose the lower bin id
      bin_id2 = ifelse(.data$y < .data$yend, .data$bin_id2, .data$bin_id), # chose the lower bin id
      y = (.data$y + .data$yend) / 2, # use mean y for sort
      support = link_width(.data$start, .data$end, .data$start2, .data$end2) *
        strand_int(combine_strands(.data$strand, .data$seq_strand, .data$seq_strand2))
    ) |>
    dplyr::group_by(.data$bin_id, .data$y) |>
    dplyr::summarize(support = sum(.data$support)) |>
    dplyr::ungroup() |>
    dplyr::filter(abs(.data$support) >= min_support) |>
    dplyr::arrange(-.data$y) |>
    dplyr::mutate(needs_flip = cumprod(strand_int(.data$support >= 0)) < 0)

  bins_to_flip <- f0 |>
    dplyr::filter(.data$needs_flip) |>
    dplyr::pull(.data$bin_id)

  if (!length(bins_to_flip)) {
    inform(str_glue(
      "All bins appear to be flipped nicely based on the given",
      "links. Maybe change `min_coverage` or flip manually"
    ))
    return(x)
  } else {
    inform(paste("Flipping:", comma(bins_to_flip)))
  }

  x %>% flip(all_of(bins_to_flip))
}

flip_impl <- function(x, bins = everything(), seqs = NULL, bin_track = seqs, seq_track = seqs) {
  # split by bin_id and select bins
  seq_tbl <- pull_seqs(x)
  seq_lst <- split_by(seq_tbl, .data$bin_id)
  # in case we want to compute selections based on a track other than seqs
  bin_sel_lst <- split_by(pull_track(x, {{ bin_track }}), .data$bin_id)
  bin_i <- tidyselect::eval_select(expr({{ bins }}), bin_sel_lst)
  if (length(bin_i) == 0) rlang::abort("no bins selected")
  # select bins to operate on
  flip_tbl <- bind_rows(seq_lst[names(bin_i)])

  # flip seqs in bins
  seqs <- enquo(seqs)
  if (!quo_is_null(seqs)) {
    seq_sel_lst <- split_by(pull_track(x, {{ seq_track }}), .data$seq_id)
    seq_sel_lst <- seq_sel_lst[names(seq_sel_lst) %in% flip_tbl$seq_id]
    seq_i <- tidyselect::eval_select(expr(!!seqs), seq_sel_lst)
    seq_i <- flip_tbl$seq_id %in% names(seq_i)
    flip_tbl$strand[seq_i] <- flip_strand(flip_tbl$strand[seq_i])
    # flip entire bins
  } else {
    flip_tbl %<>% dplyr::group_by(.data$bin_id) %>%
      dplyr::mutate(strand = flip_strand(.data$strand)) %>%
      dplyr::arrange(-row_number(), .by_group = TRUE)
  }

  # splice modified bins back into rest
  flip_lst <- flip_tbl %>% split_by(.data$bin_id)
  seq_lst[names(flip_lst)] <- flip_lst
  seq_tbl <- bind_rows(seq_lst)

  x <- set_seqs(x, seq_tbl)
  layout(x)
}

link_width <- function(start, end, start2, end2) {
  (width(start, end) + width(start2, end2)) / 2
}
