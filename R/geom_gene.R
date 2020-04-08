#' Draw genes
#'
#' Draw complex genes models. Supports prefiltering for coding regions (CDS) and
#' intron-containg genes. `geom_gene()` actually adds two layers, and is a
#' de-facto shorthand for `geom_cds() + geom_cds_hinge()`, which draw the coding
#' region polygons and the intronic connection lines,
#' respectively. `geom_gene_gggenes()` is a slightly reparameterized version of
#' `geom_gene_arrow()` of the gggenes package.
#'
#' Befault only features of `type=="CDS" are drawn. If no `type` columns is
#' provided in the feature table, all feature are considered "CDS". Change the
#' `data=use_genes(type=="CDS")` parameter to modify this behaviour.
#'
#' Features with the same `feature_id` are considered to be parts of the same
#' gene, but on different exons. They will be drawn with connecting lines,
#' i.e. hinges in between them.
#'
#' `geom_gene_gggenes()` works conceptionally different form the other gene-ish
#' geoms. It supports fixed-size height/widths, but does not support multi-exon
#' genes, and only works with a linear coordinate system.
#'
#' @inheritParams ggplot2::geom_point
#' @param height,arrow_height,arrow_width Dimensions of the arrow-shaped gene
#' polygon.
#'
#' Note: In the gggenomes default layout, the distance between two genomes on
#' the y-axis is 1. Height and arrow_height are specified in percent y-axis
#' units. Width is expressed in absolute x-axis units, i.e. number of
#' nucleotides. Due to this parameterization the behave different from common
#' fixed-size units like size and linewidth, and change dynamically with the
#' aspect-ratio.These are not fixed-size units like point size, and will change
#' with the aspect-ratio.
#' @param hinges Draw hinges (small lines) between the parts of the CDS present
#' on different exons.
geom_gene <- function(mapping = NULL, data = use_genes(type=="CDS"), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, height = 5,
                     arrow_width = 75, arrow_height = 5, hinges = TRUE, ...){
  default_aes <- aes(y=y,x=x,xend=xend,group=feature_id)
  mapping <- aes_intersect(mapping, default_aes)

  layers <- list()
  if(hinges){
    layers <- list(layer(
      geom = GeomCdsHinge, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, height=height, ...)
    ))
  }

  layers <- c(layers, layer(
    geom = GeomCds, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, height=height, arrow_width=arrow_width, arrow_height=arrow_height, ...)
  ))

  layers
}

#' @rdname geom_gene
#' @export
geom_cds <- function(mapping = NULL, data = use_genes(type=="CDS"), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, height = 5,
                     arrow_width = 100, arrow_height = 5, ...){
  default_aes <- aes(y=y,x=x,xend=xend,group=feature_id)
  mapping <- aes_intersect(mapping, default_aes)


  layer(
    geom = GeomCds, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, height=height, arrow_width=arrow_width, arrow_height=arrow_height, ...)
  )
}

#' @rdname geom_gene
#' @export
geom_cds_hinge <- function(mapping = NULL, data = use_genes(type=="CDS"), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, height = 5, ...){

  default_aes <- aes(y=y,x=x,xend=xend,group=feature_id)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    geom = GeomCdsHinge, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, height=height, ...)
  )
}

#' @export
GeomCds <-  ggproto(
  "GeomCds", Geom,
  extra_params = c("na.rm", "arrow_width", "arrow_height"),
  default_aes = aes(colour = "black", fill = "grey35", size = 0.5, linetype = 1,
    alpha = NA),
  required_aes = c("x", "xend", "y"),
  optional_aes = c("height"),
  non_missing_aes = c("ymin", "ymax"),
  setup_data = function(data, params) {
    data$height <- (data$height %||% params$height) / 100
    transform(data,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },
  draw_group = function(self, data, panel_params, coord, linejoin = "mitre", arrow_width, arrow_height) {
    aesthetics <- setdiff(names(data), c("x", "xend", "y", "ymin", "ymax", "height"))
    if(data[1,"x"] < data[1,"xend"])
      data <- data[order(data$x),]
    else
      data <- data[order(-data$x),]
    #
    n <- nrow(data)
    cds_polys <- list()
    if(n>1){
      cds_polys <- lapply(split(data[1:(n-1),], seq_len(n-1)), function(row) {
        poly <- cds2rect(row$x, row$xend, row$ymin, row$ymax)
        aes <- ggplot2:::new_data_frame(row[aesthetics])[rep(1,nrow(poly)), ]
        poly_aes <- cbind(poly, aes)
        GeomPolygon$draw_panel(poly_aes, panel_params, coord)
      })
    }
    #
    row <- data[n,]
    poly <- cds2arrow(row$x, row$xend, row$ymin, row$ymax, row$y, arrow_width, arrow_height)
    aes <- ggplot2:::new_data_frame(row[aesthetics])[rep(1,nrow(poly)), ]
    poly_aes <- cbind(poly, aes)
    cds_polys[[n]] <- GeomPolygon$draw_panel(poly_aes, panel_params, coord)
    #
    ggplot2:::ggname("gene2", do.call("grobTree", cds_polys))
  },
  draw_key = draw_key_polygon
)


cds2rect <- function(x, xend, ymin, ymax) {
  ggplot2:::new_data_frame(list(
    y = c(ymin, ymax, ymax, ymin, ymin),
    x = c(x, x, xend, xend, x)
  ))
}

cds2arrow <- function(x, xend, ymin, ymax, y, arrow_width, arrow_height) {
  amin <- y - (arrow_height/200)
  amax <- y + (arrow_height/200)

  if(abs(x-xend) <= arrow_width){

    ggplot2:::new_data_frame(list(
      x = c(x,    x,    xend, x),
      y = c(amax, amin, y,    amax)
    ))
  }else{
    xa <- if(x < xend) xend - arrow_width else xend + arrow_width
    ggplot2:::new_data_frame(list(
      x = c(x,    x,    xa,   xa,   xend, xa,   xa,   x),
      y = c(ymax, ymin, ymin, amin, y,    amax, ymax, ymax)
    ))
  }
}

#' @export
GeomCdsHinge <-  ggproto(
  "GeomCdsHinge", Geom,
  extra_params = c("na.rm"),
  default_aes = aes(colour = "black", size = 0.5, linetype = 1,
    alpha = NA),
  required_aes = c("x", "xend", "y"),
  optional_aes = c("height", "fill"), # when used with gene_gene
  non_missing_aes = c("ymin", "ymax"),
  setup_data = function(data, params) {
    data$height <- (data$height %||% params$height) / 100
    transform(data,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },
  draw_group = function(self, data, panel_params, coord, linejoin = "mitre") {
    if(nrow(data) <2){
      paths <- zeroGrob()
    }else{
      aesthetics <- setdiff(names(data), c("x", "xend", "y", "ymin", "ymax", "height"))
      paths <- cds2hinge(data,aesthetics)
      paths <- lapply(paths, function(path) GeomPath$draw_panel(path, panel_params, coord))
    }
    ggplot2:::ggname("gene2", do.call("grobTree", paths))
  },
  draw_key = draw_key_polygon
)

cds2hinge <- function(data,aesthetics){
  data <- data[order(data$x),]
  n <- nrow(data)
  forward <- data[1,"x"] < data[1,"xend"]
  a <- 2:n
  b <- a-1
  if(forward){
    x1 <- data$x[a]
    y1 <- data$y[a]
    x3 <- data$xend[b]
    y3 <- data$y[b]
  }else{
    x1 <- data$x[b]
    y1 <- data$y[b]
    x3 <- data$xend[a]
    y3 <- data$y[a]
  }
  x2 <- (x1+x3)/2
  y2 <- (data$ymax[a] + data$ymax[b])/2

  lapply(b, function(i){
    path <- ggplot2:::new_data_frame(c(
      list(x=c(x1[i],x2[i],x3[i]), y=c(y1[i], y2[i], y3[i])),
      data[i, aesthetics]))
  })
}

#' @rdname geom_gene
#' @importFrom gggenes geom_gene_arrow
#' @inheritParams gggenes::geom_gene_arrow
#' @export
geom_gene_gggenes <- function(mapping = NULL, data = use(genes),
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
