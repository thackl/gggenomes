#' Draw interactive gene models (experimental)
#'
#' Experimental variant of [geom_gene()] that renders genes as interactive
#' SVG elements using the \pkg{ggiraph} package, e.g. to show a tooltip on
#' hover when the plot is rendered with [ggiraph::girafe()]. Requires the
#' \pkg{ggiraph} package to be installed.
#'
#' @param mapping in addition to the aesthetics supported by [geom_gene()],
#'   `tooltip`, `data_id` and `onclick` are supported to control the
#'   interactive behavior of the underlying \pkg{ggiraph} elements (see
#'   [ggiraph::geom_polygon_interactive()] for details on their meaning).
#'   These are only effective when the plot is rendered with
#'   [ggiraph::girafe()]; other graphics devices silently ignore them and
#'   render the gene models exactly like [geom_gene()]. They must be mapped
#'   with [ggplot2::aes()]; passing them as fixed arguments is not supported.
#' @param ... all other arguments are passed on to [geom_gene()].
#' @export
#' @examples
#' if (requireNamespace("ggiraph", quietly = TRUE)) {
#'   p <- gggenomes(genes = emale_genes) +
#'     geom_gene_interactive(aes(fill = name, tooltip = name))
#'   ggiraph::girafe(ggobj = p)
#' }
geom_gene_interactive <- function(mapping = NULL, ...) {
  if (!requireNamespace("ggiraph", quietly = TRUE)) {
    abort("Package 'ggiraph' is required for geom_gene_interactive(). Install it with install.packages('ggiraph')")
  }

  # pull out interactive aes so geom_gene() doesn't warn about not knowing them
  ipar <- c("tooltip", "data_id", "onclick")
  interactive_aes <- mapping[intersect(names(mapping), ipar)]
  mapping[intersect(names(mapping), ipar)] <- NULL

  l <- geom_gene(mapping = mapping, ...)
  l$geom <- GeomGeneInteractive
  l$mapping[names(interactive_aes)] <- interactive_aes
  l
}

#' GeomGeneInteractive
#' @noRd
GeomGeneInteractive <- ggplot2::ggproto("GeomGeneInteractive", GeomGene,
  optional_aes = c(GeomGene$optional_aes, "tooltip", "data_id", "onclick"),
  default_aes = aes_intersect(
    aes(tooltip = NULL, data_id = NULL, onclick = NULL),
    GeomGene$default_aes
  ),
  draw_panel = function(self, data, panel_params, coord, sizes, cds_aes, rna_aes, intron_aes, intron_types) {
    # reuse GeomGene's gTree as-is (class "genetree"); just plug in ggiraph's
    # interactive polygon grob, picked up by the existing makeContent.genetree
    gt <- ggplot2::ggproto_parent(GeomGene, self)$draw_panel(
      data, panel_params, coord, sizes, cds_aes, rna_aes, intron_aes, intron_types
    )
    gt$polygon_grob <- ggiraph::interactive_polygon_grob
    gt
  }
)
