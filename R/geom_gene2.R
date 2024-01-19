geom_gene2 <- function(
    mapping = NULL, data = genes(), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, height = 2,
    cds_type = "CDS", gene_type = "gene", seg_size = 3, seg_colour = "black", seg_alpha = NA,
    seg_linetype = 1, ...) {
  # type and gene_id are magically added by use(.feats_as_genes) if they are
  # missing in the feat table
  default_aes <- aes(y = .data$y, x = .data$x, xend = .data$xend, type = .data$type, group = .data$gene_id)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    geom = GeomGene2, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, height = height, cds_type = cds_type, gene_type = gene_type,
      seg_size = seg_size, seg_colour = seg_colour, seg_alpha = seg_alpha, seg_linetype = seg_linetype, ...
    )
  )
}

GeomGene2 <- ggproto(
  "GeomGene2", Geom,
  extra_params = c("na.rm", "cds_type", "gene_type", "seg_alpha", "seg_size", "seg_linetype", "seg_colour"),
  default_aes = aes(
    colour = "black", fill = "grey35", size = 0.5, linetype = 1,
    alpha = NA, height = 2
  ),
  required_aes = c("x", "xend", "y", "type"),
  non_missing_aes = c("ymin", "ymax"),
  setup_data = function(data, params) {
    data$height <- data$height %||% params$height
    data$height <- data$height / 100
    data$type <- data$type %||% params$cds_type
    transform(data,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },
  draw_group = function(self, data, panel_params, coord, linejoin = "mitre", cds_type, gene_type,
                        seg_alpha = NA, seg_size = 3, seg_linetype = 1, seg_colour = "black") {
    if (TRUE) {
      aesthetics <- setdiff(names(data), c("x", "xend", "y", "ymin", "ymax", "type", "height"))
      # cds
      cds <- data[data$type == cds_type, ]
      cds_polys <- lapply(split(cds, seq_len(nrow(cds))), function(row) {
        poly <- gene_to_poly(row$x, row$xend, row$ymin, row$ymax)
        aes <- vctrs::data_frame(row[aesthetics])[rep(1, nrow(poly)), ]
        poly_aes <- cbind(poly, aes)
        GeomPolygon$draw_panel(poly_aes, panel_params, coord)
      })
      # draw a gene segment - we need
      # colour  y yend  x xend PANEL group size linetype alpha
      # #F8766D -1   -1 10   21     1     1    3        1    NA
      gene <- data[data$type == gene_type, ]
      gene_seg <- list(colour = seg_colour)
      if (nrow(gene) == 0) { # infer from CDS
        gene_seg$y <- sum(range(cds$y)) / 2
        gene_seg$yend <- gene_seg$y
        # need gene strand, infer from first CDS
        if (cds[1, "x"] <= cds[1, "xend"]) {
          gene_seg$x <- min(cds$x)
          gene_seg$xend <- max(cds$xend)
        } else {
          gene_seg$x <- max(cds$x)
          gene_seg$xend <- min(cds$xend)
        }
        gene_seg$PANEL <- cds[1, "PANEL"]
        gene_seg$group <- cds[1, "group"]
      } else { # draw a segment for each "gene_type" feat
        gene_seg$y <- (gene$ymin + gene$ymax) / 2
        gene_seg$yend <- gene_seg$y
        gene_seg$x <- gene$x
        gene_seg$xend <- gene$xend
        gene_seg$PANEL <- gene$PANEL
        gene_seg$PANEL <- gene$group
      }
      gene_seg$size <- seg_size
      gene_seg$linetype <- seg_linetype
      gene_seg$alpha <- seg_alpha
      gene_aes <- vctrs::data_frame(gene_seg)
      gene_segs <- GeomSegment$draw_panel(gene_aes, panel_params, coord)
      grobs <- c(list(gene_segs), cds_polys)
      ggplot2:::ggname("gene2", do.call("grobTree", grobs))
    }
  },
  draw_key = draw_key_polygon
)

gene_to_poly <- function(x, xend, ymin, ymax) {
  vctrs::data_frame(
    .name_repair = "minimal",
    y = c(ymin, ymax, ymax, ymin, ymin),
    x = c(x, x, xend, xend, x)
  )
}
