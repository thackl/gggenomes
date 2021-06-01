#' Draw gene models
#'
#' Draw coding sequences, mRNAs and other non-coding features. Supports
#' multi-exon features. CDS and mRNAs in the same group are plotted together.
#' They can therefore also be positioned as a single unit using the `position`
#' argument.
#'
#' @eval ggplot2:::rd_aesthetics("geom", "gene")
#' @section Aesthetics:
#'
#'   'type' and 'group' (mapped to 'type' and 'geom_id' by default) power the
#'   proper recognition of CDS and their corresponding mRNAs so that they can be
#'   drawn as one composite object. Overwrite 'group' to plot CDS and mRNAs
#'   independently.
#'
#'   'introns' (mapped to 'introns') is used to compute intron/exon boundaries.
#'   Use the parameter `intron_types` if you want to disable introns.
#'
#' @inheritParams ggplot2::layer
#' @param size,rna_size the size of the gene model, aka the height of the
#'   polygons. `rna_size` only applies to non-coding parts of the gene model,
#'   defaults to size.
#' @param shape,rna_shape vector of height and width of the arrow tip, defaults
#'   to size. If only one value is provided it is recycled. Set '0' to
#'   deactivates arrow-shaped tips. `rna_shape` only applies to non-coding parts
#'   of the gene model, defaults to shape.
#' @param intron_shape single value controlling the kink of the intron line.
#'   Defaults to size. Set 0 for straight lines between exons.
#' @param intron_types introns will only be computed/drawn for features with
#'   types listed here. Set to "CDS" to plot mRNAs as continous features, and
#'   set to NA to completely ignore introns.
#' @param cds_aes,rna_aes,intron_aes overwrite aesthetics for different model
#'   parts. Need to be wrapped in [ggplot2::aes()]. NOTE: These remappings are
#'   applied after the data has been transformed and mapped by the plot scales
#'   (see [ggplot2::after_scale()]). So you need to map between aesthetic names
#'   (not data columns) and with standardized names, i.e. British English
#'   spelling. These mappings can be used to dynamically change parts of the
#'   gene model. For example, to change the color of introns from a hard-coded
#'   "black" to the same color used to fill the CDS you could specify
#'   `intron_aes=aes(colour = fill)`. By default, `rna_aes` is remapped with
#'   `aes(fill=colorspace::lighten(fill, .5), colour=colorspace::lighten(colour,
#'   .5))` to give it a lighter appearence than the corresponding CDS but in the
#'   same color.
#'
#'
#' @export
#' @examples
#' gggenomes(genes=emale_genes) +
#'  geom_gene()
#'
#' gggenomes(genes=emale_genes) +
#'  geom_gene(aes(fill=as.numeric(gc_content)),position="strand") +
#'  scale_fill_viridis_b()
#'
#' types <- c(NA, "CDS", "mRNA", "tRNA", "tmRNA", "ncRNA", "rRNA", "intron",
#'  "misc_RNA", "mobile_element", "operon", "...")
#' g0 <- tibble(seq_id="A", start=seq_along(types)*10,
#'  end=seq_along(types)*10+7, type=types, introns=list(c(3,5)))
#'
#' gggenomes(genes=g0) +
#'   # all features in the "genes" regardless of type
#'   geom_feat(data=feats(genes)) +
#'   geom_text(aes(label="geom_feat", x=-15, y=.9)) + xlim(-20, NA) +
#'   # only features in the "genes" of geneish type (implicit `data=genes()`)
#'   geom_gene() +
#'   geom_gene_tag(aes(label=ifelse(is.na(type), "<NA>", type)), data=genes(.gene_types = NULL)) +
#'   geom_text(aes(label="geom_gene", x=-15, y=1)) +
#'   # control which types are returned from the track
#'   geom_gene(aes(y=1.1), data=genes(.gene_types = c("CDS", "misc_RNA"))) +
#'   geom_text(aes(label="gene_types", x=-15, y=1.1)) +
#'   # control which types can have introns
#'   geom_gene(aes(y=1.2, yend=1.2), data=genes(.gene_types = c("CDS", "misc_RNA")),  intron_types = "misc_RNA") +
#'   geom_text(aes(label="intron_types", x=-15, y=1.2))
#'
#' # spliced genes
#' library(patchwork)
#' g0 <- read_gff3(ex("eden-utr.gff"))
#' gg <- gggenomes(genes=g0)
#' gg + geom_gene(position="pile") +
#' gg + geom_gene(aes(fill=type), position="pile",
#'          shape = 0, intron_shape = 0, color="white") +
#' # some fine-control on cds/rna/intron after_scale aesthetics
#' gg + geom_gene(aes(fill=geom_id), position="pile",
#'          size = 2, shape = c(4,3), rna_size = 2, intron_shape = 4, stroke=0,
#'          cds_aes=aes(fill="black"), rna_aes=aes(fill=fill),
#'          intron_aes=aes(colour=fill, stroke=2)) +
#'      scale_fill_viridis_d() +
#' # fun with introns
#' gg + geom_gene(aes(fill=geom_id), position="pile", size = 3, shape=c(4,4)) +
#' gg + geom_gene(aes(fill=geom_id), position="pile", size = 3, shape=c(4,4),
#'          intron_types=c()) +
#' gg + geom_gene(aes(fill=geom_id), position="pile", size = 3, shape=c(4,4),
#'          intron_types="CDS")
geom_gene <- function(mapping = NULL, data = genes(), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    size = 2, rna_size = size, shape = size, rna_shape = shape, intron_shape = size,
    intron_types = c("CDS", "mRNA", "tRNA", "tmRNA", "ncRNA", "rRNA"),
    cds_aes = NULL, rna_aes = NULL, intron_aes = NULL, ...){

  sizes <- c(size_expand(size, shape), size_expand(rna_size, rna_shape), intron=intron_shape)

  default_aes <- aes(y=y, x=x, xend=xend,type=type, introns=introns, group=geom_id)
  mapping <- aes_intersect(mapping, default_aes)

  cds_def <- aes()
  cds_aes <- aes_intersect(cds_aes, cds_def)

  rna_def <- aes(
    fill = colorspace::lighten(fill, .5),
    color = colorspace::lighten(colour, .5))
  rna_aes <- aes_intersect(rna_aes, rna_def)

  intron_def <- aes(colour = "black", stroke = .4)
  intron_aes <- aes_intersect(intron_aes, intron_def)

  layer(
    geom = GeomGene, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, sizes=sizes, cds_aes=cds_aes, rna_aes=rna_aes,
        intron_aes=intron_aes, intron_types=intron_types, ...)
  )
}

size_expand <- function(...){
  x <- c(...)
  n <- length(x)
  if (n > 3) abort("at most 3 values supported")
  else if (n == 3) x
  else if (n == 2) x[c(1,2,2)]
  else x[c(1,1,1)]

}


#' GeomGene
#' @noRd
GeomGene <- ggplot2::ggproto("GeomGene", ggplot2::Geom,
  required_aes = c("x", "xend", "y"),
  optional_aes = c("type", "introns"),
  default_aes = ggplot2::aes(
    alpha = 1,
    colour = "black",
    fill = "cornsilk3",
    stroke = .4,
    linetype = 1,
    type = "CDS",
    introns = NULL
  ),
  draw_key = function(data, params, size) {
    grid::polygonGrob(
      x = c(1,6,9,6,1)/10, y = c(8,8,5,2,2)/10, id.lengths = 5,
      gp = grid::gpar(
        fill = data$fill %||% "cornflowerblue",
        col = data$colour %||% "black",
        lty = data$linetype %||% 1,
        lwd = (data$stroke %||% .4) * ggplot2::.pt
      )
    )
  },
  setup_data = function(data, params){
    # unnest exons before coord$transform so all x/xend get transformed
    data <- mutate(data,
      id = row_number(),
      introns = ifelse(type %in% params$intron_types, introns, list(NULL)),
      introns = map(introns, ~.x - c(1, 0)) # convert 1[s,e] to 0[s,e) for drawing
    )

    data <- unnest_exons(data)
  },
  draw_panel = function(self, data, panel_params, coord, sizes, cds_aes, rna_aes, intron_aes, intron_types){
    if(!coord$is_linear()){
      abort(paste("geom_gene() only works with Cartesian coordinates.",
                  "Use geom_gene_seg() or geom_gene2() instead."))
    }

    # need to compute all exon spans before transformation!
    # see setup_data
    data <- coord$transform(data, panel_params)

    # after-scale modify cds/rna aes
    rna_data <- filter(data, !type %in% "CDS") # != misses NA
    rna_data <- mutate(rna_data, !!!rna_aes)
    cds_data <- filter(data, type == "CDS")
    cds_data <- mutate(cds_data, !!!cds_aes)

    data <- bind_rows(cds_data, rna_data)

    # after-scale modify other aes
    data <- mutate(data,
      # convert to alpha hex color: color fill
      across(c(fill, colour), ~map2_chr(.x, alpha, ggplot2::alpha)),
      # convert to pt: stroke
      stroke = stroke * ggplot2::.pt
    )

    gt <- grid::gTree(
      data = data,
      cl = "genetree",
      sizes = sizes,
      intron_aes = intron_aes
    )
    gt$name <- grid::grobName(gt, "geom_gene")
    gt
  }
)

native_height <- function(x){
  as.numeric(grid::convertHeight(grid::unit(x, "mm"), "native")) /2
}

native_width <- function(x){
  as.numeric(grid::convertWidth(grid::unit(x, "mm"), "native")) /2
}

#' @export
makeContent.genetree <- function(x){
  data <- x$data

  coord_flipped <- FALSE
  if(names(data)[1] == "x"){
    coord_flipped <- TRUE
    data <- rename(data, y=x, x=y, xend=yend)
  }

  s <- x$sizes
  height <- native_height(s[1])
  arrow_height <- native_height(s[2])
  arrow_width <- native_width(s[3])
  rna_height <- native_height(s[4])
  rna_arrow_height <- native_height(s[5])
  rna_arrow_width <- native_width(s[6])
  intron_height <- native_height(s[7])

  grobs <- list()

  # CDS
  cds_exons <- tibble()
  cds_data <- data %>% filter(type == "CDS")
  if(nrow(cds_data) > 0){
    cds_exons <- cds_data %>% group_by(id) %>% summarize(
      across(c(-x, -xend, -y), first),
      exons = list(exon_polys(x, xend, y, height, arrow_width, arrow_height)))
  }

  # RNA (mRNA, tRNA)
  rna_exons <- tibble()
  rna_data <- data %>% filter(type != "CDS")
  if(nrow(rna_data) > 0){
      rna_exons <- rna_data %>% group_by(id) %>% summarize(
        across(c(-x, -xend, -y), first),
        exons = list(exon_polys(x, xend, y, rna_height, rna_arrow_width, rna_arrow_height)))
  }

  # one grob per feature for feature-wise aes (all exons same)
  all_exons <- bind_rows(rna_exons, cds_exons)
  grobs <- pmap(all_exons, function(exons, fill, colour, linetype, stroke, ...){
    grid::polygonGrob(x = exons$x, y = exons$y, id = exons$id,
        gp = grid::gpar(fill = fill, col = colour, lty = linetype, lwd = stroke))
  })

  if(nrow(data) > 0){
    rna_introns <- data %>% group_by(group) %>%
      # remove CDS if group has mRNA
      filter(type != (if("mRNA" %in% type) "CDS" else "!bogus")) %>%
      group_by(id) %>% filter(n() > 1) %>% summarize(
        across(c(-x, -xend, -y), first),
        introns = list(intron_polys(x, xend, y, intron_height)))

    # after-scale modify intron aes
    rna_introns <- mutate(rna_introns, !!!x$intron_aes,
        # recomp. alpha b/c colour modification can strip it
        colour = ggplot2::alpha(colour, alpha),
        stroke = stroke * ggplot2::.pt
        )

    grobs <- c(pmap(rna_introns, function(introns, colour, alpha, linetype, stroke, ...){
      grid::polylineGrob(
        x = introns$x,
        y = introns$y,
        id = introns$id,
        gp = grid::gpar(
          col = colour,
          lty = linetype,
          lwd = stroke,
          lineend = "butt",
          linejoin = "round"
        ))
    }), grobs)
  }

  if(coord_flipped){
    grobs <- map(grobs, function(x){x[1:2] <- x[2:1]; x})
  }

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}

exon_spans <- function(x, xend, introns, ...){
  n <- length(introns)
  if(n<2){
    return(tibble(x=x, xend=xend))
  }

  introns <- if(x<xend) x + introns else xend + rev(introns)
  exons <- c(x, introns, xend)

  as_tibble(vec_unzip(exons, c("x", "xend")))
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

#' @export
unnest_exons <- function(x){
  rowwise(x) %>%
  mutate(exons = list(exon_spans(x, xend, introns)),
         x=NULL, xend=NULL, introns=NULL) %>%
  unnest(exons)
}
