#' draw seqs
#'
#' @param data seq_layout
#' @param arrow set to non-NULL to generate default arrows
#' @inheritParams ggplot2::geom_segment
#' @importFrom ggplot2 geom_segment
#' @export
geom_seq <- function(mapping = NULL, data = use(seqs),
    arrow = NULL, ...){

  default_aes <- aes(x, y, xend=xend, yend=y)
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
    arrow = NULL, nudge_by_strand = NULL, size = 3, ...){

  default_aes <- aes(x, y, xend=xend, yend=y)
  mapping <- aes_intersect(mapping, default_aes)
  mapping <- aes_nudge_by_strand(mapping, nudge_by_strand)

  # default arrow
  if (!rlang::is_null(arrow) & !inherits(arrow, "arrow"))
    arrow <- grid::arrow(length = unit(3, "pt"))

  # would be cleaner with GeomFeature ggproto...
  if (has_name(mapping, "size")) size <- NULL

  geom_segment(mapping = mapping, data = data, arrow = arrow, size = size, ...)
}

#' draw genes
#'
#' @param data feature_layout
#' @inheritParams gggenes::geom_gene_arrow
#' @importFrom gggenes geom_gene_arrow
#' @export
geom_gene <- function(mapping = NULL, data = use(genes),
    nudge_by_strand = NULL, arrowhead_width = grid::unit(2, "mm"),
    arrowhead_height = grid::unit(3, "mm"),
    arrow_body_height = grid::unit(3, "mm"), ...){

  default_aes <- aes(y=y,xmin=x,xmax=xend)
  mapping <- aes_intersect(mapping, default_aes)
  mapping <- aes_nudge_by_strand(mapping, nudge_by_strand, "y")

  gggenes::geom_gene_arrow(mapping = mapping, data = data,
    arrowhead_width=arrowhead_width, arrowhead_height=arrowhead_height,
    arrow_body_height=arrow_body_height, ...)
}

#' draw links
#'
#' @param data link_layout
#' @param array set to non-NULL to generate default arrows
#' @inheritParams ggplot2::geom_polygon
#' @importFrom ggplot2 geom_polygon
#' @export
geom_link <- function(mapping = NULL, data = use(links), nudge_frac=.1, ...){

  default_aes <- aes(x=x, y=y + .nudge_sign * {{nudge_frac}}, group=.lix)
  mapping <- aes_intersect(mapping, default_aes)

  geom_polygon(mapping = mapping, data=data, ...)
}

#' draw feature labels
#'
#' @export
geom_seq_label <- function(mapping = NULL, data = use(seqs),
    hjust = 0, nudge_y = -0.1, size = 6, ...){

  default_aes <- aes_(y=~y,x=~x, label=~seq_id)
  mapping <- aes_intersect(mapping, default_aes)

  geom_text(mapping = mapping, data = data, hjust = hjust,
            nudge_y = nudge_y, size = size, ...)
}

#' draw feature labels
#'
#' @export
geom_gene_label <- function(mapping = NULL, data = use(genes),
    angle = 45,hjust = 0, nudge_y = 0.1, size = 6, ...){

  default_aes <- aes_(y=~y,x=~(x+xend)/2)
  mapping <- aes_intersect(mapping, default_aes)

  geom_text(mapping = mapping, data = data, angle = angle, hjust = hjust,
            nudge_y = nudge_y, size = size, ...)
}
#' @export
geom_feature_label <- function(mapping = NULL, data = use(features),
    angle = 45,hjust = 0, nudge_y = 0.1, size = 6, ...){

  default_aes <- aes_(y=~y,x=~(x+xend)/2)
  mapping <- aes_intersect(mapping, default_aes)

  geom_text(mapping = mapping, data = data, angle = angle, hjust = hjust,
            nudge_y = nudge_y, size = size, ...)
}

#' draw link labels
#'
#' @export
geom_link_label <- function(mapping = NULL, data = use(links, .pix==1),
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
