#' Draw links between genomes
#'
#' @description Draws connections between genomes, such as genome/gene/protein
#'   alignments and gene/protein clusters. `geom_link()` draws links as filled
#'   polygons, `geom_link_line()` draws a single connecting line.
#'
#'   Note that by default only links between adjacent genomes are computed and
#'   shown. To compute and show all links between all genomes, set
#'   `gggenomes(..., adjacent_only=FALSE)`.
#'
#' @details The function calls upon the data stored within the `link` track.
#'   Data frames added to this track have `seq_id` and `seq_id2` as required
#'   variables. Optional and recommended variables include `start`, `start2`,
#'   `end`, `end2`, `bin_id`, `bin_id2` and `strand`.
#'
#'   Note, when start/end is not specified, links will be created between the
#'   entire contigs of `seq_id` and `seq_id2`.
#'
#' @inheritParams ggplot2::geom_polygon
#' @param offset distance between seq center and link start. Use two values
#'   `c(<offset_top>, <offset_bottom>)` for different top and bottom offsets
#' @return A ggplot2 layer with links.
#' @export
#' @examples
#' p0 <- gggenomes(seqs = emale_seqs, links = emale_ava) + geom_seq()
#'
#' # default links
#' p1 <- p0 + geom_link()
#'
#' # change offset from seqs and color
#' p2 <- p0 + geom_link(aes(fill = de, color = de), offset = 0.05) +
#'   scale_fill_viridis_b() + scale_colour_viridis_b()
#'
#' # combine with flip
#' p3 <- p0 |> flip(3, 4, 5) +
#'   geom_link()
#'
#' # compute & show all links among all genomes
#' # usually not useful and not recommended for large dataset
#' p4 <- gggenomes(links = emale_ava, adjacent_only = FALSE) + geom_link()
#'
#' library(patchwork) # combine plots in one figure
#' p1 + p2 + p3 + p4 + plot_layout(nrow = 1)
geom_link <- function(
    mapping = NULL, data = links(), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    offset = 0.15, ...) {
  if (length(offset) == 1) offset <- offset[c(1, 1)]

  default_aes <- aes(y = .data$y, x = .data$x, xend = .data$xend, yend = .data$yend, xmin = .data$xmin, xmax = .data$xmax)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    geom = GeomLink, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, offset = offset, ...)
  )
}
#' @rdname geom_link
#' @export
#' @examples
#' q0 <- gggenomes(emale_genes, emale_seqs) |>
#'   add_clusters(emale_cogs) +
#'   geom_seq() + geom_gene()
#'
#' # link gene clusters with polygon
#' q1 <- q0 + geom_link(aes(fill = cluster_id))
#'
#' # link gene clusters with lines
#' q2 <- q0 + geom_link_line(aes(color = cluster_id))
#'
#' q1 + q2 + plot_layout(nrow = 1, guides = "collect")
#'
geom_link_line <- function(
    mapping = NULL, data = links(), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    ...) {
  default_aes <- aes(y = .data$y, yend = .data$yend, x = (.data$x + .data$xend) / 2, xend = (.data$xmin + .data$xmax) / 2)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    geom = GeomSegment, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomLink <- ggproto(
  "GeomLink", Geom,
  default_aes = aes(
    colour = "honeydew3", fill = "honeydew3", size = 0.5, linetype = 1,
    alpha = 0.7
  ),
  required_aes = c("x", "xend", "y", "xmin", "xmax", "yend"),
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre", offset = c(0.15, 0.15)) {
    if (TRUE) { # !coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "xend", "y", "xmin", "xmax", "yend")
      )
      polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
        poly <- link_to_poly(row$x, row$xend, row$y, row$xmin, row$xmax, row$yend, offset)
        aes <- vctrs::data_frame(row[aesthetics])[rep(1, 5), ]
        GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
      })

      ggplot2__ggname("link", do.call("grobTree", polys))
    }
  },
  draw_key = draw_key_polygon
)

link_to_poly <- function(x, xend, y, xmin, xmax, yend, offset) {
  if (y > yend) {
    y <- y - offset[1]
    yend <- yend + offset[2]
  } else {
    y <- y + offset[2]
    yend <- yend - offset[1]
  }

  vctrs::data_frame(
    .name_repair = "minimal",
    y = c(y, y, yend, yend, y),
    x = c(x, xend, xmax, xmin, x)
  )
}
