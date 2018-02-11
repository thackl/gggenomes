#' draw contigs
#' 
#' @param data contig_layout
#' @param array set to non-NULL to generate default arrows
#' @inheritParams ggplot::geom_segment
#' @export
geom_contig <- function(mapping = NULL, data = NULL, arrow = NULL, ...){

    mapping <- aesIntersect(mapping, aes_(x=~cstart, xend=~cend, y=~gix, yend=~gix))

    # default arrow
    if (!is_null(arrow) & !inherits(arrow, "array"))
        arrow <- arrow(length = unit(3, "pt")) 

    geom_segment(mapping = mapping, data=data, arrow = arrow, ...)
}

#' draw links
#' 
#' @param data link_layout
#' @inheritParams ggplot::geom_polygon
#' @param array set to non-NULL to generate default arrows
#' @export
geom_link <- function(mapping = NULL, data = NULL, nudge_frac=.1, ...){
    mapping <- aesIntersect(mapping, aes_(x=~x, y=quo(gix + nudge_sign * !!nudge_frac), group=~lix))

    geom_polygon(mapping = mapping, data=data, ...)
}

