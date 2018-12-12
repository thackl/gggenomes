#' draw contigs
#'
#' @param data contig_layout
#' @param arrow set to non-NULL to generate default arrows
#' @inheritParams ggplot2::geom_segment
#' @importFrom ggplot2 geom_segment
#' @export
geom_contig <- function(mapping = NULL, data = use(contigs),
    arrow = NULL, ...){

  default_aes <- aes(.x, .y, xend=.xend, yend=.y)
  mapping <- aes_intersect(mapping, default_aes)

  # default arrow
  if (!rlang::is_null(arrow) & !inherits(arrow, "arrow"))
        arrow <- grid::arrow(length = unit(3, "pt"))

  geom_segment(mapping = mapping, data = data, arrow = arrow, ...)
}

#' draw features
#'
#' @param data feature_layout
#' @export
geom_feature <- function(mapping = NULL, data = use(features),
                         arrow = NULL, nudge_by_strand = NULL, ...){

  default_aes <- aes(.x, .y, xend=.xend, yend=.y, size=3)
  mapping <- aes_intersect(mapping, default_aes)
  mapping <- aes_nudge_by_strand(mapping, nudge_by_strand)

  # default arrow
  if (!rlang::is_null(arrow) & !inherits(arrow, "arrow"))
    arrow <- grid::arrow(length = unit(3, "pt"))

  geom_segment(mapping = mapping, data = data, arrow = arrow, ...)
}

#' draw genes
#'
#' @param data feature_layout
#' @inheritParams gggenes::geom_gene_arrow
#' @importFrom gggenes geom_gene_arrow
#' @export
geom_gene <- function(mapping = NULL, data = use(genes),
    nudge_by_strand = NULL, ...){

  default_aes <- aes(y=.y,xmin=.x,xmax=.xend)
  mapping <- aes_intersect(mapping, default_aes)
  mapping <- aes_nudge_by_strand(mapping, nudge_by_strand, "y")

  gggenes::geom_gene_arrow(mapping = mapping, data = data, ...)
}

#' draw links
#'
#' @param data link_layout
#' @param array set to non-NULL to generate default arrows
#' @inheritParams ggplot2::geom_polygon
#' @importFrom ggplot2 geom_polygon
#' @export
geom_link <- function(mapping = NULL, data = use(links), nudge_frac=.1, ...){

  default_aes <- aes(x=x, y=.gix + .nudge_sign * !!nudge_frac, group=.lix)
  mapping <- aes_intersect(mapping, default_aes)

  geom_polygon(mapping = mapping, data=data, ...)
}

#' draw feature labels
#'
#' @export
geom_gene_label <- function(mapping = NULL, data = use(genes),
    angle = 45,hjust = 0, nudge_y = 0.2, size = 2, ...){

  default_aes <- aes_(y=~y,x=~(x+xend)/2)
  mapping <- aes_intersect(mapping, default_aes)

  geom_text(mapping = mapping, data = data, angle = angle, hjust = hjust,
            nudge_y = nudge_y, size = size, ...)
}
#' @export
geom_feature_label <- function(mapping = NULL, data = use(features),
    angle = 45,hjust = 0, nudge_y = 0.2, size = 2, ...){

  default_aes <- aes_(y=~y,x=~(x+xend)/2)
  mapping <- aes_intersect(mapping, default_aes)

  geom_text(mapping = mapping, data = data, angle = angle, hjust = hjust,
            nudge_y = nudge_y, size = size, ...)
}
