#' draw seqs
#'
#' @description
#' `geom_seq()` draws contigs for each sequence/chromosome supplied in the `seqs` track.
#' Several sequences belonging to the same bin will be plotted next to one another.
#'
#' If `seqs` track is empty, sequences are inferred from the `feats` or `links` track respectively.
#'
#' (*The length of sequences can be deduced from the axis and is typically indicated in base pairs.*)
#'
#' @details
#' `geom_seq()` uses `ggplot2::geom_segment()` under the hood. As a result,
#' different aesthetics such as *alpha*, *linewidth*, *color*, etc.
#' can be called upon to modify the visualization of the data.
#'
#' Note: The `seqs` track indicates the length/region of the sequence/contigs that will be plotted.
#' *Feats* or *links* data that falls outside of this region are ignored!
#' @return Sequence data drawn as contigs is added as a layer/component to the plot.
#'
#' @param data seq_layout: Uses the first data frame stored in the `seqs` track, by default.
#' @param arrow set to non-NULL to generate default arrows
#' @inheritParams ggplot2::geom_segment
#' @importFrom ggplot2 geom_segment
#' @export
#' @examples
#' # Simple example of geom_seq
#' gggenomes(seqs = emale_seqs) +
#'   geom_seq() + # creates contigs
#'   geom_bin_label() # labels bins/sequences
#'
#' # No sequence information supplied, will inform/warn that seqs are inferred from feats.
#' gggenomes(genes = emale_genes) +
#'   geom_seq() + # creates contigs
#'   geom_gene() + # draws genes on top of contigs
#'   geom_bin_label() # labels bins/sequences
#'
#' # Sequence data controls what sequences and/or regions will be plotted.
#' # Here one sequence is filtered out, Notice that the genes of the removed
#' # sequence are silently ignored and thus not plotted.
#' missing_seqs <- emale_seqs |>
#'   dplyr::filter(seq_id != "Cflag_017B") |>
#'   dplyr::arrange(seq_id) # `arrange` to restore alphabetical order.
#'
#' gggenomes(seqs = missing_seqs, genes = emale_genes) +
#'   geom_seq() + # creates contigs
#'   geom_gene() + # draws genes on top of contigs
#'   geom_bin_label() # labels bins/sequences
#'
#' # Several sequences belonging to the same *bin* are plotted next to one another
#' seqs <- tibble::tibble(
#'   bin_id = c("A", "A", "A", "B", "B", "B", "B", "C", "C"),
#'   seq_id = c("A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2"),
#'   start = c(0, 100, 200, 0, 50, 150, 250, 0, 400),
#'   end = c(100, 200, 400, 50, 100, 250, 300, 300, 500),
#'   length = c(100, 100, 200, 50, 50, 100, 50, 300, 100)
#' )
#'
#' gggenomes(seqs = seqs) +
#'   geom_seq() +
#'   geom_bin_label() + # label bins
#'   geom_seq_label() # label individual sequences
#'
#' # Wrap bins uptill a certain amount.
#' gggenomes(seqs = seqs, wrap = 300) +
#'   geom_seq() +
#'   geom_bin_label() + # label bins
#'   geom_seq_label() # label individual sequences
#'
#' # Change the space between sequences belonging to one bin
#' gggenomes(seqs = seqs, spacing = 100) +
#'   geom_seq() +
#'   geom_bin_label() + # label bins
#'   geom_seq_label() # label individual sequences
geom_seq <- function(
    mapping = NULL, data = seqs(),
    arrow = NULL, ...) {
  default_aes <- aes(.data$x, .data$y, xend = .data$xend, yend = .data$y)
  mapping <- aes_intersect(mapping, default_aes)

  # default arrow
  if (!rlang::is_null(arrow) & !inherits(arrow, "arrow")) {
    arrow <- grid::arrow(length = unit(3, "pt"))
  }

  geom_segment(mapping = mapping, data = data, arrow = arrow, ...)
}

#' Draw seq labels
#' @description
#' This function will put labels at each individual sequence.
#' By default it will plot the `seq_id` as label, but users are able to change this manually.
#'
#' Position of the label/text can be adjusted with the different arguments (e.g. `vjust`, `hjust`, `angle`, etc.)
#'
#' @details
#' This labeling function uses [ggplot2::geom_text()] under the hood.
#' Any changes to the aesthetics of the text can be performed in a ggplot2 manner.
#'
#'
#' @inheritParams geom_gene_text
#' @examples
#' # example data
#' seqs <- tibble::tibble(
#'   bin_id = c("A", "A", "A", "B", "B", "B", "B", "C", "C"),
#'   seq_id = c("A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2"),
#'   start = c(0, 100, 200, 0, 50, 150, 250, 0, 400),
#'   end = c(100, 200, 400, 50, 100, 250, 300, 300, 500),
#'   length = c(100, 100, 200, 50, 50, 100, 50, 300, 100)
#' )
#'
#' # example plot using geom_seq_label
#' gggenomes(seqs = seqs) +
#'   geom_seq() +
#'   geom_seq_label()
#'
#' # changing default label to `length` column
#' gggenomes(seqs = seqs) +
#'   geom_seq() +
#'   geom_seq_label(aes(label = length))
#'
#' # with horizontal adjustment
#' gggenomes(seqs = seqs) +
#'   geom_seq() +
#'   geom_seq_label(hjust = -5)
#'
#' # with wrapping at 300
#' gggenomes(seqs = seqs, wrap = 300) +
#'   geom_seq() +
#'   geom_seq_label()
#' @param size of the label
#' @return Sequence labels are added as a text layer/component to the plot.
#' @export
geom_seq_label <- function(
    mapping = NULL, data = seqs(),
    hjust = 0, vjust = 1, nudge_y = -0.15, size = 2.5, ...) {
  default_aes <- aes(y = .data$y, x = pmin(.data$x, .data$xend), label = .data$seq_id)
  mapping <- aes_intersect(mapping, default_aes)

  geom_text(
    mapping = mapping, data = data, hjust = hjust,
    vjust = vjust, nudge_y = nudge_y, size = size, ...
  )
}

#' Draw bin labels
#'
#' Put bin labels left of the sequences. `nudge_left` adds space relative to the
#' total bin width between the label and the seqs, by default 5%. `expand_left`
#' expands the plot to the left by 20% to make labels visible.
#'
#' Set `x` and `expand_x` to an absolute position to align all labels at a
#' specific location
#'
#' @inheritParams ggplot2::geom_text
#' @param hjust Moves the text horizontally
#' @param size of the label
#' @param nudge_left by this much relative to the widest bin
#' @param expand_left by this much relative to the widest bin
#' @param expand_x expand the plot to include this absolute x value
#' @param expand_aes provide custom aes mappings for the expansion (advanced)
#' @param yjust for multiline bins set to 0.5 to center labels on bins, and 1 to
#'   align labels to the bottom.
#' @return Bin labels are added as a text layer/component to the plot.
#' @export
#' @examples
#' s0 <- read_seqs(list.files(ex("cafeteria"), "Cr.*\\.fa.fai$", full.names = TRUE))
#' s1 <- s0 %>% dplyr::filter(length > 5e5)
#'
#' gggenomes(emale_genes) + geom_seq() + geom_gene() +
#'   geom_bin_label()
#'
#' # make larger labels and extra room on the canvas
#' gggenomes(emale_genes) + geom_seq() + geom_gene() +
#'   geom_bin_label(size = 7, expand_left = .4)
#'
#' # align labels for wrapped bins:
#' # top
#' gggenomes(seqs = s1, infer_bin_id = file_id, wrap = 5e6) +
#'   geom_seq() + geom_bin_label() + geom_seq_label()
#'
#' # center
#' gggenomes(seqs = s1, infer_bin_id = file_id, wrap = 5e6) +
#'   geom_seq() + geom_bin_label(yjust = .5) + geom_seq_label()
#'
#' # bottom
#' gggenomes(seqs = s1, infer_bin_id = file_id, wrap = 5e6) +
#'   geom_seq() + geom_bin_label(yjust = 1) + geom_seq_label()
geom_bin_label <- function(
    mapping = NULL, data = bins(), hjust = 1, size = 3,
    nudge_left = 0.05, expand_left = 0.20, expand_x = NULL, expand_aes = NULL,
    yjust = 0, ...) {
  default_aes <- aes_(
    y = ~ ymin * yjust + ymax * (1 - yjust),
    x = ~ pmin(x, xend) - max_width(x, xend) * nudge_left, label = ~bin_id
  )
  mapping <- aes_intersect(mapping, default_aes)
  r <- list(geom_text(
    mapping = mapping, data = data,
    hjust = hjust, size = size, ...
  ))

  if (!is.null(expand_x)) {
    r[[2]] <- expand_limits(x = expand_x)
  } else if (!is.na(expand_left)) {
    expand_aes <- NULL
    default_expand_aes <- aes_(y = ~y, x = ~ x - abs(min(x) - max(xend)) * expand_left)
    expand_aes <- aes_intersect(expand_aes, default_expand_aes)
    r[[2]] <- geom_blank(mapping = expand_aes, data = data)
  }
  r
}
#' Draw feat/link labels
#'
#' @description
#' These `geom_..._label()` functions able the user to plot labels/text at individual features and/or links.
#' Users have to indicate how to label the features/links by specifying `label = ...` or `aes(label = ...`
#'
#' Position of labels can be adjusted with arguments such as `vjust`, `hjust`, `angle`, `nudge_y`, etc.
#' Also check out [gggenomes::geom_bin_label()], [gggenomes::geom_seq_label()] or [gggenomes::geom_feat_text()] given their resemblance.
#'
#' @details
#' These labeling functions use [ggplot2::geom_text()] under the hood.
#' Any changes to the aesthetics of the text can be performed in a ggplot2 manner.
#'
#' @inheritParams geom_gene_text
#' @param size of the label
#' @return Gene labels are added as a text layer/component to the plot.
#' @export
geom_gene_label <- function(
    mapping = NULL, data = genes(),
    angle = 45, hjust = 0, nudge_y = 0.1, size = 6, ...) {
  default_aes <- aes_(y = ~y, x = ~ (x + xend) / 2)
  mapping <- aes_intersect(mapping, default_aes)

  geom_text(
    mapping = mapping, data = data, angle = angle, hjust = hjust,
    nudge_y = nudge_y, size = size, ...
  )
}
#' @rdname geom_gene_label
#' @export
geom_feat_label <- function(
    mapping = NULL, data = feats(),
    angle = 45, hjust = 0, nudge_y = 0.1, size = 6, ...) {
  default_aes <- aes_(y = ~y, x = ~ (x + xend) / 2)
  mapping <- aes_intersect(mapping, default_aes)

  geom_text(
    mapping = mapping, data = data, angle = angle, hjust = hjust,
    nudge_y = nudge_y, size = size, ...
  )
}

#' @rdname geom_gene_label
#' @param repel use ggrepel to avoid overlaps
#' @export
geom_link_label <- function(
    mapping = NULL, data = links(),
    angle = 0, hjust = 0.5, vjust = 0.5, size = 4, repel = FALSE, ...) {
  default_aes <- aes_(y = ~.y_center, x = ~.x_center)
  mapping <- aes_intersect(mapping, default_aes)


  if (repel) {
    ggrepel::geom_text_repel(
      mapping = mapping, data = data, angle = angle, hjust = hjust,
      vjust = vjust, size = size, ...
    )
  } else {
    geom_text(
      mapping = mapping, data = data, angle = angle, hjust = hjust,
      vjust = vjust, size = size, ...
    )
  }
}
