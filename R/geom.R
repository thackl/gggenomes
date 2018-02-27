#' draw chromosomes
#'
#' @param data contig_layout
#' @param arrow set to non-NULL to generate default arrows
#' @inheritParams ggplot2::geom_segment
#' @importFrom ggplot2 geom_segment
#' @export
geom_chromosomes <- function(mapping = NULL, data = expose_data(chromosomes),
    arrow = NULL, ...){

  default_aes <- aes_(x=~x,xend=~xend,y=~y,yend=~y)
  mapping <- aes_intersect(mapping, default_aes)

  # default arrow
  if (!is_null(arrow) & !inherits(arrow, "arrow"))
        arrow <- arrow(length = unit(3, "pt"))

  geom_segment(mapping = mapping, data = data, arrow = arrow, ...)
}

#' draw features
#'
#' @param data feature_layout
#' @export
geom_feature <- function(mapping = NULL, data = expose_data(features),
                         arrow = NULL, nudge_by_strand = NULL, ...){

  default_aes <- aes_(x=~x, xend=~xend, y=~y, yend=~y, size=3)
  mapping <- aes_intersect(mapping, default_aes)
  mapping <- aes_nudge_by_strand(mapping, nudge_by_strand)

  # default arrow
  if (!is_null(arrow) & !inherits(arrow, "arrow"))
    arrow <- arrow(length = unit(3, "pt"))

  geom_segment(mapping = mapping, data = data, arrow = arrow, ...)
}

#' draw genes
#'
#' @param data feature_layout
#' @inheritParams gggenes::geom_gene_arrow
#' @importFrom gggenes geom_gene_arrow
#' @export
geom_gene <- function(mapping = NULL, data = expose_data(features),
    nudge_by_strand = NULL, ...){

  default_aes <- aes_(y=~y,xmin=~x,xmax=~xend)
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
geom_link <- function(mapping = NULL, data = expose_data(links), nudge_frac=.1, ...){

  default_aes <- aes_(x=~x, y=quo(gix + nudge_sign * !!nudge_frac), group=~lix)
  mapping <- aes_intersect(mapping, default_aes)

  geom_polygon(mapping = mapping, data=data, ...)
}

#' draw feature labels
#'
#' @export
geom_feature_label <- function(mapping = NULL, data = expose_data(features),
    angle = 45,hjust = 0, nudge_y = 0.2, size = 2, ...){

  default_aes <- aes_(y=~y,x=~(x+xend)/2)
  mapping <- aes_intersect(mapping, default_aes)

  geom_text(mapping = mapping, data = data, angle = angle, hjust = hjust,
            nudge_y = nudge_y, size = size, ...)
}
