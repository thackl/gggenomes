#' Text
#'
#'
#' @inheritParams ggplot2::geom_text
#' @export
geom_feat_text <- function(mapping = NULL, data = feats(), stat="identity", position="identity",
                           ..., parse = FALSE, check_overlap = FALSE, na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE){

  default_aes <- aes(y=y,x=x,xend=xend)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFeatText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_feat_text
#' @export
geom_feat_tag <- function(mapping = NULL, data = feats(), stat="identity", position="identity",
                          hjust = 0, vjust = 0, angle = 45, nudge_y = .03, xjust = 0.5, strandwise = TRUE, ...,
                          parse = FALSE, check_overlap = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){

  geom_feat_text(mapping=mapping, data=data, stat=stat, position=position,
                 hjust=hjust, vjust=vjust, angle=angle, nudge_y=nudge_y, xjust=xjust, strandwise=strandwise,
                 ..., parse=parse, check_overlap=check_overlap, na.rm=na.rm, show.legend=show.legend,
                 inherit.aes=inherit.aes)
}

#' @rdname geom_feat_text
#' @export
geom_feat_note <- function(mapping = NULL, data = feats(), stat="identity", position="identity",
                           hjust = 0, vjust = 1, nudge_y = -.03, xjust = 0, strandwise = FALSE, ...,
                           parse = FALSE, check_overlap = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){

  geom_feat_text(mapping=mapping, data=data, stat=stat, position=position,
                 hjust=hjust, vjust=vjust, nudge_y=nudge_y, xjust=xjust, strandwise=strandwise, ...,
                 parse=parse, check_overlap=check_overlap, na.rm=na.rm, show.legend=show.legend,
                 inherit.aes=inherit.aes)
}

#' @rdname geom_feat_text
#' @export
geom_gene_text <- function(mapping = NULL, data = genes(), stat="identity", position="identity",
                           ..., parse = FALSE, check_overlap = FALSE, na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE){

  default_aes <- aes(y=y,x=x,xend=xend,type=type,group=geom_id)
  mapping <- aes_intersect(mapping, default_aes)

  geom_feat_text(mapping=mapping, data=data, stat=stat, position=position, ...,
                 parse=parse, check_overlap=check_overlap, na.rm=na.rm, show.legend=show.legend,
                 inherit.aes=inherit.aes)
}

#' @rdname geom_feat_text
#' @export
geom_gene_tag <- function(mapping = NULL, data = genes(), stat="identity", position="identity",
    hjust = 0, vjust = 0, angle = 45, nudge_y = .03, xjust = 0.5, strandwise = TRUE, ...,
    parse = FALSE, check_overlap = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){

  geom_gene_text(mapping=mapping, data=data, stat=stat, position=position,
      hjust=hjust, vjust=vjust, angle=angle, nudge_y=nudge_y, xjust=xjust, strandwise=strandwise,
      ..., parse=parse, check_overlap=check_overlap, na.rm=na.rm, show.legend=show.legend,
      inherit.aes=inherit.aes)
}

#' @rdname geom_feat_text
#' @export
geom_gene_note <- function(mapping = NULL, data = genes(), stat="identity", position="identity",
    hjust = 0, vjust = 1, nudge_y = -.03, xjust = 0, strandwise = FALSE, ...,
    parse = FALSE, check_overlap = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){

  geom_gene_text(mapping=mapping, data=data, stat=stat, position=position,
     hjust=hjust, vjust=vjust, nudge_y=nudge_y, xjust=xjust, strandwise=strandwise, ...,
     parse=parse, check_overlap=check_overlap, na.rm=na.rm, show.legend=show.legend,
     inherit.aes=inherit.aes)
}



#' @export
GeomFeatText <- ggproto("GeomFeatText", Geom,
  required_aes = c("x", "xend", "y", "label"),
  optional_aes = c("type"),
  default_aes = aes(
    colour = "black", size = 2.5, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = 1, family = "", fontface = 1, lineheight = 1.2
  ),
  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE,
                        nudge_x = 0, nudge_y = 0, xjust = 0.5, strandwise = FALSE) {
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    # nudge
    if(strandwise){
      data$nudge_x <- ifelse(data$x>data$xend, nudge_x * -1, nudge_x)
    }else{
      data$nudge_x <- nudge_x
      data <- swap_if(data, x>xend, x, xend)
    }

    # need to keep x/xend as is for position pile. Use xmin for text anchor -
    # needs to be known x-aes for coord$transform
    data$xmin <- data$x * (1-xjust) + data$xend * xjust
    data$xmin <- data$xmin + data$nudge_x
    data$y <- data$y + nudge_y

    # transform
    data <- coord$transform(data, panel_params)

    # just
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$xmin)
    }

    textGrob(
      lab,
      data$xmin, data$y, default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  },

  draw_key = draw_key_text
)

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
