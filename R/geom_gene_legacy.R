# Legacy implementation of geom_gene(), kept for reference and as a fallback.
# It creates one polygonGrob per feature and unnests exons rowwise for every
# feature, which gets slow for large numbers of genes. geom_gene() now uses a
# batched implementation (see geom_gene.R). Shared helpers (size_expand(),
# native_height(), exon_polys(), intron_polys(), unnest_exons(), ...) live in
# geom_gene.R.

#' Draw gene models (legacy implementation)
#'
#' Legacy version of [geom_gene()] that draws one polygon per feature. It is
#' considerably slower for large numbers of genes, but kept as a fallback.
#'
#' @inheritParams geom_gene
#' @return A ggplot2 layer with genes.
#' @keywords internal
#' @export
geom_gene_legacy <- function(
    mapping = NULL, data = genes(), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    size = 2, rna_size = size, shape = size, rna_shape = shape, intron_shape = size,
    intron_types = c("CDS", "mRNA", "tRNA", "tmRNA", "ncRNA", "rRNA"),
    cds_aes = NULL, rna_aes = NULL, intron_aes = NULL, ...) {
  sizes <- c(size_expand(size, shape), size_expand(rna_size, rna_shape), intron = intron_shape)

  default_aes <- aes(y = .data$y, x = .data$x, xend = .data$xend, type = .data$type, introns = .data$introns, group = .data$geom_id)
  mapping <- aes_intersect(mapping, default_aes)

  cds_def <- aes()
  cds_aes <- aes_intersect(cds_aes, cds_def)

  rna_def <- aes(
    fill = colorspace::lighten(fill, .5),
    color = colorspace::lighten(.data$colour, .5)
  )
  rna_aes <- aes_intersect(rna_aes, rna_def)

  intron_def <- aes(colour = "black", stroke = .4)
  intron_aes <- aes_intersect(intron_aes, intron_def)

  layer(
    geom = GeomGeneLegacy, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, sizes = sizes, cds_aes = cds_aes, rna_aes = rna_aes,
      intron_aes = intron_aes, intron_types = intron_types, ...
    )
  )
}


#' GeomGeneLegacy
#' @noRd
GeomGeneLegacy <- ggplot2::ggproto("GeomGeneLegacy", ggplot2::Geom,
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
      x = c(1, 6, 9, 6, 1) / 10, y = c(8, 8, 5, 2, 2) / 10, id.lengths = 5,
      gp = grid::gpar(
        fill = data$fill %||% "cornflowerblue",
        col = data$colour %||% "black",
        lty = data$linetype %||% 1,
        lwd = (data$stroke %||% .4) * ggplot2::.pt
      )
    )
  },
  setup_data = function(data, params) {
    # unnest exons before coord$transform so all x/xend get transformed
    data <- mutate(data,
      id = row_number(),
      introns = ifelse(type %in% params$intron_types, introns, list(NULL)),
      introns = purrr::map(introns, ~ .x - c(1, 0)) # convert 1[s,e] to 0[s,e) for drawing
    )

    data <- unnest_exons(data)
  },
  draw_panel = function(self, data, panel_params, coord, sizes, cds_aes, rna_aes, intron_aes, intron_types) {
    if (!coord$is_linear()) {
      abort(paste(
        "geom_gene_legacy() only works with Cartesian coordinates.",
        "Use geom_gene_seg() or geom_gene2() instead."
      ))
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
      across(c(fill, colour), ~ purrr::map2_chr(.x, alpha, ggplot2::alpha)),
      # convert to pt: stroke
      stroke = stroke * ggplot2::.pt
    )

    gt <- grid::gTree(
      data = data,
      cl = "genetree_legacy",
      sizes = sizes,
      intron_aes = intron_aes
    )
    gt$name <- grid::grobName(gt, "geom_gene_legacy")
    gt
  }
)

#' @export
makeContent.genetree_legacy <- function(x) {
  data <- x$data

  coord_flipped <- FALSE
  if (names(data)[1] == "x") {
    coord_flipped <- TRUE
    data <- rename(data, y = "x", x = "y", xend = "yend")
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
  cds_data <- data %>% filter(.data$type == "CDS")
  if (nrow(cds_data) > 0) {
    cds_exons <- cds_data %>%
      dplyr::group_by(.data$id) %>%
      dplyr::summarize(
        dplyr::across(c(-x, -xend, -y), first),
        exons = list(exon_polys(.data$x, .data$xend, .data$y, height, arrow_width, arrow_height))
      )
  }

  # RNA (mRNA, tRNA)
  rna_exons <- tibble()
  rna_data <- data %>% filter(.data$type != "CDS")
  if (nrow(rna_data) > 0) {
    rna_exons <- rna_data %>%
      dplyr::group_by(.data$id) %>%
      dplyr::summarize(
        dplyr::across(c(-x, -xend, -y), first),
        exons = list(exon_polys(.data$x, .data$xend, .data$y, rna_height, rna_arrow_width, rna_arrow_height))
      )
  }

  # one grob per feature for feature-wise aes (all exons same)
  all_exons <- bind_rows(rna_exons, cds_exons)
  grobs <- purrr::pmap(all_exons, function(exons, fill, colour, linetype, stroke, ...) {
    grid::polygonGrob(
      x = exons$x, y = exons$y, id = exons$id,
      gp = grid::gpar(fill = fill, col = colour, lty = linetype, lwd = stroke)
    )
  })

  if (nrow(data) > 0) {
    rna_introns <- data %>%
      dplyr::group_by(.data$group) %>%
      # remove CDS if group has mRNA
      dplyr::filter(.data$type != (if ("mRNA" %in% .data$type) "CDS" else "!bogus")) %>%
      dplyr::group_by(.data$id) %>%
      dplyr::filter(n() > 1) %>%
      dplyr::summarize(
        dplyr::across(c(-x, -xend, -y), first),
        introns = list(intron_polys(.data$x, .data$xend, .data$y, intron_height))
      )

    # after-scale modify intron aes
    rna_introns <- mutate(rna_introns, !!!x$intron_aes,
      # recomp. alpha b/c colour modification can strip it
      colour = ggplot2::alpha(.data$colour, alpha),
      stroke = .data$stroke * ggplot2::.pt
    )

    grobs <- c(purrr::pmap(rna_introns, function(introns, colour, alpha, linetype, stroke, ...) {
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
        )
      )
    }), grobs)
  }

  if (coord_flipped) {
    grobs <- purrr::map(grobs, function(x) {
      x[1:2] <- x[2:1]
      x
    })
  }

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
