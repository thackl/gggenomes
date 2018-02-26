#' draw contigs
#'
#' @param data contig_layout
#' @param arrow set to non-NULL to generate default arrows
#' @inheritParams ggplot2::geom_segment
#' @importFrom ggplot2 geom_segment
#' @export
geom_contig <- function(mapping = NULL, data = expose_data(contigs),
    stat = StatOffsetLength, arrow = NULL, ...){

    default_aes <- aes_(y=~gix, yend=~gix, offset=~goffset+coffset,
                        length=~clength, strand=~gstrand*cstrand)
    mapping <- aesIntersect(mapping, default_aes)

    # default arrow
    if (!is_null(arrow) & !inherits(arrow, "arrow"))
        arrow <- arrow(length = unit(3, "pt"))

    geom_segment(mapping = mapping, data = data, stat = stat, arrow = arrow, ...)
}

#' @export
geom_chromosomes <- function(mapping = NULL, data = expose_data(chromosomes),
    arrow = NULL, ...){

    default_aes <- aes_(x=~x,xend=~xend,y=~y,yend=~y)
    mapping <- aesIntersect(mapping, default_aes)

    # default arrow
    if (!is_null(arrow) & !inherits(arrow, "arrow"))
        arrow <- arrow(length = unit(3, "pt"))

    geom_segment(mapping = mapping, data = data, arrow = arrow, ...)
}

#' draw features
#'
#' @param data feature_layout
geom_feature <- function(mapping = NULL, data = expose_data(features),
    stat=StatOffsetRange, arrow = NULL, ...){

    default_aes <- aes_(y=~gix, yend=~gix, offset=~foffset,
                        start=~fstart, end=~fend, strand=~fstrand)
    mapping <- gggenomes:::aesIntersect(mapping, default_aes)

    # default arrow
    if (!is_null(arrow) & !inherits(arrow, "arrow"))
        arrow <- arrow(length = unit(3, "pt"))

    geom_segment(mapping = mapping, data = data, stat = stat, arrow = arrow, ...)
}

#' draw genes
#'
#' @param data feature_layout
#' @inheritParams gggenes::geom_gene_arrow
#' @importFrom gggenes geom_gene_arrow
#' @export
geom_gene <- function(mapping = NULL, data = expose_data(features),
    stat = StatOffsetGene, ...){

    default_aes <- aes_(y=~gix,start=~fstart,end=~fend,offset=~foffset,strand=~fstrand)
    mapping <- aesIntersect(mapping, default_aes)

    gggenes::geom_gene_arrow(mapping = mapping, data = data, stat = stat, ...)
}

#' draw links
#'
#' @param data link_layout
#' @param array set to non-NULL to generate default arrows
#' @inheritParams ggplot2::geom_polygon
#' @importFrom ggplot2 geom_polygon
#' @export
geom_link <- function(mapping = NULL, data = expose_data(links), nudge_frac=.1, ...){
    write('# TODO: StatOffset', stderr())
    mapping <- aesIntersect(mapping, aes_(x=~x, y=quo(gix + nudge_sign * !!nudge_frac), group=~lix))

    geom_polygon(mapping = mapping, data=data, ...)
}

#' draw feature labels
#'
#' @export
geom_feature_label <- function(mapping = NULL, data = expose_data(features),
    stat = StatOffsetRange, angle = 45,hjust = 0, nudge_y = 0.2, size = 2, ...){

    default_aes <- aes_(y=~gix,start=~(fstart+fend)/2,offset=~foffset,strand=1)
    mapping <- aesIntersect(mapping, default_aes)

    geom_text(mapping = mapping, data = data, stat = stat,
        angle = angle, hjust = hjust, nudge_y = nudge_y, size = size, ...)
}
