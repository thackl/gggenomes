#' draw contigs
#' 
#' @param data contig_layout
#' @param array set to non-NULL to generate default arrows
#' @inheritParams ggplot2::geom_segment
#' @importFrom ggplot2 geom_segment
#' @export
geom_contig <- function(mapping = NULL, data = expose_data(contigs), arrow = NULL, ...){

    default_aes <- aes_(y=~gix, yend=~gix, offset=~goffset+coffset,
                        length=~clength, strand=~gstrand*cstrand)
    mapping <- aesIntersect(mapping, default_aes)

    # default arrow
    if (!is_null(arrow) & !inherits(arrow, "array"))
        arrow <- arrow(length = unit(3, "pt")) 

    geom_segment(mapping = mapping, data = data, arrow = arrow, stat=StatOffsetLength, ...)
}

#' draw links
#' 
#' @param data link_layout
#' @inheritParams ggplot2::geom_polygon
#' @param array set to non-NULL to generate default arrows
#' @importFrom ggplot2 geom_polygon
#' @export
geom_link <- function(mapping = NULL, data = expose_data(links), nudge_frac=.1, ...){
    mapping <- aesIntersect(mapping, aes_(x=~x, y=quo(gix + nudge_sign * !!nudge_frac), group=~lix))

    geom_polygon(mapping = mapping, data=data, ...)
}

