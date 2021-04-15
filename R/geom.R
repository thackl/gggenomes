#' draw seqs
#'
#' @param data seq_layout
#' @param arrow set to non-NULL to generate default arrows
#' @inheritParams ggplot2::geom_segment
#' @importFrom ggplot2 geom_segment
#' @export
geom_seq <- function(mapping = NULL, data = seqs(),
    arrow = NULL, ...){

  default_aes <- aes(x, y, xend=xend, yend=y)
  mapping <- aes_intersect(mapping, default_aes)

  # default arrow
  if (!rlang::is_null(arrow) & !inherits(arrow, "arrow"))
        arrow <- grid::arrow(length = unit(3, "pt"))

  geom_segment(mapping = mapping, data = data, arrow = arrow, ...)
}

#' draw feat labels
#'
#' @export
geom_seq_label <- function(mapping = NULL, data = seqs(),
    hjust = 0, vjust = 1, nudge_y = -0.15, size = 2.5, ...){

  default_aes <- aes(y=y,x=pmin(x,xend), label=seq_id)
  mapping <- aes_intersect(mapping, default_aes)

  geom_text(mapping = mapping, data = data, hjust = hjust,
            vjust = vjust, nudge_y = nudge_y, size = size, ...)
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
#' @param nudge_left by this much relative to the widest bin
#' @param expand_left by this much relative to the widest bin
#' @param expand_x expand the plot to include this absolute x value
#' @param expand_aes provide custom aes mappings for the expansion (advanced)
#' @param yjust for multiline bins set to 0.5 to center labels on bins, and 1 to
#'   align labels to the bottom.
#' @export
#' @examples
#' s0 <- read_seqs(list.files(ex("cafeteria"), "Cr.*\\.fa", full.names = TRUE))
#' s1 <- s0 %>% filter(length>5e5)
#'
#' gggenomes(emale_genes) + geom_seq() + geom_gene() +
#'   geom_bin_label()
#'
#' # make larger labels and extra room on the canvas
#' gggenomes(emale_genes) + geom_seq() + geom_gene() +
#'   geom_bin_label(size = 7, expand_left =.4)
#'
#' # align labels for wrapped bins:
#' # top
#' gggenomes(seqs=s1, infer_bin_id=file_id, wrap=5e6) +
#'   geom_seq() + geom_bin_label() + geom_seq_label()
#'
#' # center
#' gggenomes(seqs=s1, infer_bin_id=file_id, wrap=5e6) +
#'   geom_seq() + geom_bin_label(yjust=.5) + geom_seq_label()
#'
#' # bottom
#' gggenomes(seqs=s1, infer_bin_id=file_id, wrap=5e6) +
#'   geom_seq() + geom_bin_label(yjust=1) + geom_seq_label()
geom_bin_label <- function(mapping = NULL, data=bins(), hjust = 1, size = 3,
    nudge_left = 0.05, expand_left = 0.20, expand_x=NULL, expand_aes=NULL,
    yjust = 0, ...){

  default_aes <- aes_(y=~ymin * yjust + ymax * (1-yjust),
                      x=~pmin(x,xend) - max_width(x,xend) * nudge_left, label=~bin_id)
  mapping <- aes_intersect(mapping, default_aes)
  r <- list(geom_text(mapping = mapping, data = data,
              hjust = hjust, size = size, ...))

  if(!is.null(expand_x)){
    r[[2]] <- expand_limits(x=expand_x)
  }else if(!is.na(expand_left)){
    expand_aes <- NULL
    default_expand_aes <- aes_(y=~y,x=~x - abs(min(x)-max(xend)) * expand_left)
    expand_aes <- aes_intersect(expand_aes, default_expand_aes)
    r[[2]] <- geom_blank(mapping = expand_aes, data = data)
  }
  r
}
#' draw feat labels
#'
#' @export
geom_gene_label <- function(mapping = NULL, data = genes(),
    angle = 45,hjust = 0, nudge_y = 0.1, size = 6, ...){

  default_aes <- aes_(y=~y,x=~(x+xend)/2)
  mapping <- aes_intersect(mapping, default_aes)

  geom_text(mapping = mapping, data = data, angle = angle, hjust = hjust,
            nudge_y = nudge_y, size = size, ...)
}
#' @export
geom_feat_label <- function(mapping = NULL, data = feats(),
    angle = 45,hjust = 0, nudge_y = 0.1, size = 6, ...){

  default_aes <- aes_(y=~y,x=~(x+xend)/2)
  mapping <- aes_intersect(mapping, default_aes)

  geom_text(mapping = mapping, data = data, angle = angle, hjust = hjust,
            nudge_y = nudge_y, size = size, ...)
}

#' draw link labels
#'
#' @export
geom_link_label <- function(mapping = NULL, data = links(),
    angle = 0,hjust = 0.5, vjust = 0.5, size = 4, repel=FALSE, ...){

  default_aes <- aes_(y=~.y_center,x=~.x_center)
  mapping <- aes_intersect(mapping, default_aes)


  if(repel){
    ggrepel::geom_text_repel(mapping = mapping, data = data, angle = angle, hjust = hjust,
        vjust = vjust, size = size, ...)
  }else{
    geom_text(mapping = mapping, data = data, angle = angle, hjust = hjust,
        vjust = vjust, size = size, ...)
  }
}
