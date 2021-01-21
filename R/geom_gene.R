#' Draw genes
#'
#' @export
geom_gene <- function(mapping = NULL, data = genes(), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    arrow_size = c(3,3,3), chevrons = NULL, ...){

  if(length(arrow_size) == 1) arrow_size <- arrow_size[c(1,1,1)]
  if(length(arrow_size) == 2) arrow_size <- arrow_size[c(1,2,2)]

  default_aes <- aes(y=y,x=x,xend=xend,type=type,introns=introns,group=geom_id)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    geom = GeomGene, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, height = arrow_size[1], arrow_height = arrow_size[2],
    arrow_width = arrow_size[3], chevrons = chevrons, ...)
  )
}


#' GeomGene
#' @noRd
GeomGene <- ggplot2::ggproto("GeomGene", ggplot2::Geom,
  required_aes = c("x", "xend", "y"),
  optional_aes = c("type", "introns"),
  default_aes = ggplot2::aes(
    alpha = 1,
    colour = "black",
    fill = "white",
    linetype = 1,
    size = 0.3,
    type = "CDS",
    introns = NULL
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
  setup_data = function(data, params){
    # unnest exons prior to coord$transform so all x/xend get projected
    data %>%
      mutate(id=row_number()) %>%
      rowwise %>%
      mutate(exons = list(exon_spans(x, xend, introns)),
             x=NULL, xend=NULL, introns=NULL) %>%
      unnest(exons)
  },
  draw_panel = function(self, data, panel_params, coord, height=3, arrow_width=2, arrow_height=3,
      chevrons=NULL){
    if(!coord$is_linear()){
      abort(paste("geom_gene() only works with Cartesian coordinates.",
                  "Use geom_gene_seg() or geom_gene2() instead."))
    }

    # need to compute all exon spans before transformation!
    # see setup_data
    data <- coord$transform(data, panel_params)
    gt <- grid::gTree(
      data = data,
      cl = "genetree",
      height = height,
      arrow_width = arrow_width,
      arrow_height = arrow_height
    )
    gt$name <- grid::grobName(gt, "geom_gene")
    gt
  }
)

#' @export
makeContent.genetree <- function(x){
  data <- x$data
  height <- as.numeric(grid::convertHeight(grid::unit(x$height, "mm"), "native")) /2
  arrow_height <- as.numeric(grid::convertHeight(grid::unit(x$arrow_height, "mm"), "native")) /2
  arrow_width <- as.numeric(grid::convertWidth(grid::unit(x$arrow_width, "mm"), "native")) /2

  grobs <- list()


  # CDS
  cds_data <- data %>% filter(type == "CDS")
  if(nrow(cds_data) > 0){
    cds_exons <- cds_data %>% group_by(id) %>% summarize(
      across(c(-x, -xend, -y), first),
      exons = list(exon_polys(x, xend, y, height, arrow_width, arrow_height)))

    # one grob per feature for feature-wise aes (all exons same)
    grobs <- c(grobs, pmap(cds_exons, function(exons, fill, colour, alpha, linetype, size, ...){
      grid::polygonGrob(
        x = exons$x,
        y = exons$y,
        id = exons$id,
        gp = grid::gpar(
          fill = ggplot2::alpha(fill, alpha),
          col = ggplot2::alpha(colour, alpha),
          lty = linetype,
          lwd = size * ggplot2::.pt
        ))
      }))
  }

  # RNA (mRNA, tRNA)
  rna_data <- data %>% filter(type != "CDS")
  if(nrow(rna_data) > 0){
    rna_exons <- rna_data %>% group_by(id) %>% summarize(
      across(c(-x, -xend, -y), first),
      exons = list(exon_polys(x, xend, y, height * 0.7, arrow_width * 0.7, arrow_height*0.7)))

    # one grob per feature for feature-wise aes (all exons same)
    grobs <- c(pmap(rna_exons, function(exons, fill, colour, alpha, linetype, size, ...){
      grid::polygonGrob(
        x = exons$x,
        y = exons$y,
        id = exons$id,
        gp = grid::gpar(
          fill = ggplot2::alpha(fill, alpha * .5),
          #col = ggplot2::alpha(fill, alpha),
          lty = linetype,
          lwd = size * ggplot2::.pt
        ))
    }), grobs)
  }

  if(nrow(data) > 0){
    rna_introns <- data %>% group_by(group) %>%
      # remove CDS if group has mRNA
      filter(type != (if("mRNA" %in% type) "CDS" else "!bogus")) %>%
      group_by(id) %>% filter(n() > 1) %>% summarize(
        across(c(-x, -xend, -y), first),
        introns = list(intron_polys(x, xend, y, height)))

    grobs <- c(pmap(rna_introns, function(introns, fill, colour, alpha, linetype, size, ...){
      grid::polylineGrob(
        x = introns$x,
        y = introns$y,
        id = introns$id,
        # TODO: par not working
        gp = grid::gpar(
          col = ggplot2::alpha("black", 1),
          lty = linetype,
          lwd = size * ggplot2::.pt
        ))
    }), grobs)
  }

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}

unzip <- function(x, names=NULL){
  i <- c(TRUE, FALSE)
  set_names(list(x[i], x[!i]), names)
}

exon_spans <- function(x, xend, introns, ...){
  n <- length(introns)
  if(n<2){
    return(tibble(x=x, xend=xend))
  }

  introns <- if(x<xend) x + introns else xend + rev(introns)
  exons <- c(x, introns, xend)

  as_tibble(unzip(exons, c("x", "xend")))
}

exon_polys <- function(x, xend, y, height, arrow_width, arrow_height){
  n <- length(x)
  # arrow poly
  polys <- tibble(id="0", !!!span2arrow(x[n], xend[n], y[n], height, arrow_width, arrow_height))
  # rect polys
  if(n>1)
    polys <- bind_rows(pmap_df(.id="id", list(x[-n], xend[-n], y[-n]), span2rect, height), polys)
  polys
}

span2rect <- function(x, xend, y, height){
  tibble(
    x = c(x, x, xend, xend),
    y = c(y-height, y+height, y+height, y-height)
  )
}

span2arrow <- function(x, xend, y, height, arrow_width, arrow_height) {
  ymin <- y - height
  ymax <- y + height
  amin <- y - arrow_height
  amax <- y + arrow_height
  #
  if(abs(x-xend) <= arrow_width){
    tibble(
      x = c(x,    x,    xend, x),
      y = c(amax, amin, y,    amax)
    )
  }else{
    xa <- if(x < xend) xend - arrow_width else xend + arrow_width
    tibble(
      x = c(x,    x,    xa,   xa,   xend, xa,   xa,   x),
      y = c(ymax, ymin, ymin, amin, y,    amax, ymax, ymax)
    )
  }
}

intron_polys <- function(x, xend, y, height){
  n <- length(x)
  if (n<2) return(NULL)
  a <- 2:n
  b <- a-1

  x1 <- xend[b]
  y1 <- y[b]
  x3 <- x[a]
  y3 <- y[a]

  x2 <- (x1+x3)/2
  y2 <- (y[a]+height + y[b]+height)/2

  tibble(
    x=c(rbind(x1,x2,x3)),
    y=c(rbind(y1,y2,y3)),
    id=rep(1:(n-1), each=3)
  )
}
