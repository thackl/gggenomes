#' Draw genes
geom_gene <- function(mapping = NULL, data = use_genes(1, type == "CDS"), stat = "identity",
    position = position_stack1(grouped=TRUE), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    arrow_size = c(3,3,3), chevrons = NULL, ...){

  if(length(arrow_size) == 1) arrow_size <- arrow_size[c(1,1,1)]
  if(length(arrow_size) == 2) arrow_size <- arrow_size[c(1,2,2)]

  default_aes <- aes(y=y,x=x,xend=xend,group=feature_id)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    geom = GeomCdsArrow, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, height = arrow_size[1], arrow_height = arrow_size[2],
    arrow_width = arrow_size[3], chevrons = chevrons, ...)
  )
}


#' GeomGeneArrow
#' @noRd
GeomCdsArrow <- ggplot2::ggproto("GeomCdsArrow", ggplot2::Geom,
  required_aes = c("x", "xend", "y"),
  default_aes = ggplot2::aes(
    alpha = 1,
    colour = "black",
    fill = "white",
    linetype = 1,
    size = 0.3
  ),
  draw_key = function(data, params, size) {
    grid::rectGrob(
      width = grid::unit(1, "npc") - grid::unit(1, "mm"),
      height = grid::unit(1, "npc") - grid::unit(1, "mm"),
      gp = grid::gpar(
        col = data$colour,
        fill = ggplot2::alpha(data$fill, data$alpha),
        lty = data$linetype,
        lwd = data$size * ggplot2::.pt
      )
    )
  },
  draw_group = function(self, data, panel_params, coord, height=3, arrow_width=2, arrow_height=3,
      chevrons=NULL){
    if(!coord$is_linear()){
      abort(paste("geom_gene() only works with Cartesian coordinates.",
                  "Use geom_gene_seg() or geom_gene2() instead."))
    }
    chevron_pars <- chevrons
    draw_chevrons <- TRUE
    if(!is.list(chevron_pars)){
      if(is.null(chevron_pars) | isTRUE(chevron_pars)) chevron_pars <- list()
      else if(is.na(chevron_pars) | isFALSE(chevron_pars)) draw_chevrons <- FALSE
      else abort("'chevrons' needs to be one of NULL,TRUE,NA,FALSE or a list of parameters")
      chevron_pars <- list()
    }
    names(chevron_pars)[names(chevron_pars) == "color"] <- "colour"
    for (aes in c("size","linetype","colour","alpha")){
      if(is.null(chevron_pars[[aes]])) chevron_pars[[aes]] <- data[[aes]][1]
    }
    # sort to get
    if(data[1,"x"] < data[1,"xend"])
      data <- data[order(data$x),]
    else
      data <- data[order(-data$x),]
    #
    data <- coord$transform(data, panel_params)
    gt <- grid::gTree(
      data = data,
      cl = "cdsarrowtree",
      height = height,
      arrow_width = arrow_width,
      arrow_height = arrow_height,
      draw_chevrons = draw_chevrons,
      chevron_size = chevron_pars$size,
      chevron_linetype = chevron_pars$linetype,
      chevron_colour = chevron_pars$colour,
      chevron_alpha = chevron_pars$alpha
    )
    gt$name <- grid::grobName(gt, "geom_cds_arrow")
    gt
  }
)

makeContent.cdsarrowtree <- function(x){
  data <- x$data
  n <- nrow(data)
  height <- as.numeric(grid::convertHeight(grid::unit(x$height, "mm"), "native")) /2
  arrow_height <- as.numeric(grid::convertHeight(grid::unit(x$arrow_height, "mm"), "native")) /2
  arrow_width <- as.numeric(grid::convertWidth(grid::unit(x$arrow_width, "mm"), "native")) /2

  # last exon (arrow-poly)
  exon <- data[n,]
  arrow_coords <- cds2arrow(exon$x, exon$xend, exon$y, height, arrow_width, arrow_height)
  arrow_coords$id <- rep(n,length(arrow_coords$x))
  arrow_coords <- list(arrow_coords)

  # other exons (rect-poly)
  if(n>1){
    rects_coords <- lapply(1:(n-1), function(i) {
      exon <- data[i,]
      rect_coord <- cds2rect(exon$x, exon$xend, exon$y, height)
      rect_coord$id <- rep(i, each=4)
      rect_coord
    })
    arrow_coords <- c(rects_coords, arrow_coords)
  }

  # one grob per exon for exon-wise aes
  grobs <- lapply(1:length(arrow_coords), function(i){
    exon <- arrow_coords[[i]]
    grid::polygonGrob(
      x = exon$x,
      y = exon$y,
      id = exon$id,
      gp = grid::gpar(
        fill = ggplot2::alpha(data$fill[i], data$alpha[i]),
        col = ggplot2::alpha(data$colour[i], data$alpha[i]),
        lty = data$linetype[i],
        lwd = data$size[i] * ggplot2::.pt
      )
    )
  })

  if(x$draw_chevrons && n>1){
    chev_coords <- cds2chevron(data$x, data$xend, data$y, height)
    chevs <- grid::polylineGrob(
      x = chev_coords$x,
      y = chev_coords$y,
      id = chev_coords$id,
      # TODO: par not working
      gp = grid::gpar(
        col = ggplot2::alpha(x$chevron_colour, x$chevron_alpha),
        lty = x$chevron_linetype,
        lwd = x$chevron_size * ggplot2::.pt
      )
    )
    grobs <- c(list(chevs), grobs)
  }

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}


cds2arrow <- function(x, xend,  y, height, arrow_width, arrow_height) {
  ymin <- y - height
  ymax <- y + height
  amin <- y - arrow_height
  amax <- y + arrow_height
  #
  coords <- if(abs(x-xend) <= arrow_width){
    list(
      x = c(x,    x,    xend, x),
      y = c(amax, amin, y,    amax)
    )
  }else{
    xa <- if(x < xend) xend - arrow_width else xend + arrow_width
    list(
      x = c(x,    x,    xa,   xa,   xend, xa,   xa,   x),
      y = c(ymax, ymin, ymin, amin, y,    amax, ymax, ymax)
    )
  }
  coords
}

cds2rect <- function(x, xend, y, height) {
  ymin <- y - height
  ymax <- y + height
  list(
    y = c(ymin, ymax, ymax, ymin),
    x = c(x, x, xend, xend)
  )
}

cds2chevron <- function(x, xend, y, height){
  n <- length(x)
  a <- 2:n
  b <- a-1

  x1 <- xend[b]
  y1 <- y[b]
  x3 <- x[a]
  y3 <- y[a]

  x2 <- (x1+x3)/2
  y2 <- (y[a]+height + y[b]+height)/2

  list(
    x=c(rbind(x1,x2,x3)),
    y=c(rbind(y1,y2,y3)),
    id=rep(1:(n-1), each=3)
  )
}
