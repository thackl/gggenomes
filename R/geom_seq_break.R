#' Decorate truncated sequences
#'
#' @description
#' `geom_seq_break()` adds decorations to the ends of truncated sequences. These
#' could arise from zooming onto sequence loci with `focus()`, or manually
#' annotating sequences with start > 1 and/or end < length.
#' @param label the character to decorate ends with. Provide two values for
#' different start and end decorations, e.g. `label=c("]", "[")`.
#' @param data_start seq_layout of sequences for which to decorate the start.
#' default: `seqs(start >1)`
#' @param data_end seq_layout of sequences for which to decorate the end.
#' default: `seqs(end < length)`
#' @inheritParams ggplot2::geom_text
#' @importFrom ggplot2 geom_text
#' @export
#' @examples
#' # decorate breaks created with focus()
#' gggenomes(emale_genes, emale_seqs) |>
#'   focus(.expand=1e3, .max_dist = 1e3) +
#'   geom_seq() + geom_gene() +
#'   geom_seq_break()
#'
#' # customize decorations
#' gggenomes(emale_genes, emale_seqs) |>
#'   focus(.expand=1e3, .max_dist = 1e3) +
#'   geom_seq() + geom_gene() +
#'   geom_seq_break(label=c("[", "]"), size=3, color="#1b9e77")
#'
#' # decorate manually truncated sequences
#' s0 <- tibble::tribble(
#'   # start/end define regions, i.e. truncated contigs
#'   ~bin_id, ~seq_id, ~length, ~start, ~end,
#'   "complete_genome", "chromosome_1_long_trunc_2side", 1e5, 1e4, 2.1e4,
#'   "fragmented_assembly", "contig_1_trunc_1side", 1.3e4, .9e4, 1.3e4,
#'   "fragmented_assembly", "contig_2_short_complete", 0.3e4, 1, 0.3e4,
#'   "fragmented_assembly", "contig_3_trunc_2sides", 2e4, 1e4, 1.4e4
#' )
#'
#' l0 <- tibble::tribble(
#'   ~seq_id, ~start, ~end, ~seq_id2, ~start2, ~end2,
#'   "chromosome_1_long_trunc_2side", 1.1e4, 1.4e4,
#'   "contig_1_trunc_1side", 1e4, 1.3e4,
#'   "chromosome_1_long_trunc_2side", 1.4e4, 1.7e4,
#'   "contig_2_short_complete", 1, 0.3e4,
#'   "chromosome_1_long_trunc_2side", 1.7e4, 2e4,
#'   "contig_3_trunc_2sides", 1e4, 1.3e4
#' )
#'
#' gggenomes(seqs=s0, links=l0) +
#'   geom_seq() + geom_link() +
#'   geom_seq_label(nudge_y=-.05) +
#'   geom_seq_break()
geom_seq_break <- function(mapping_start = NULL, mapping_end = NULL,
                       data_start = seqs(start > 1), data_end=seqs(end < length),
                       label="/", size=4, hjust=.75, family="bold", stat="identity",
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...){

  label_start <- label[1]
  label_end <- label[length(label)]

  aes_start <- aes(x=x, y=y)
  aes_start <- gggenomes:::aes_intersect(mapping_start, aes_start)

  aes_end <- aes(x=xend, y=y)
  aes_end <- gggenomes:::aes_intersect(mapping_end, aes_end)

  list(
    geom_text(aes_start, data=data_start, label=label_start, size=size, hjust=hjust,
              family=family, ...),
    geom_text(aes_end, data=data_end, label=label_end, size=size, hjust=1-hjust,
              family=family, ...)
  )
}

