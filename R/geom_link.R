#' Draw links between genomes
#'
#' @description Draw connections between genomes, such as genome/gene/protein
#'   alignments and gene/protein clusters. `geom_link()` and
#'   `geom_link_curved()` create filled polygons between regions,
#'   `geom_link_line()` a single connecting line.
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
#' @param curve curvature of the link. If `NA` or `0`, the link edges will be
#'   straight. For curved links, higher values lead to stronger curvature.
#'   Typical values are between `5` and `15`.
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
#' 
#' q0 <- gggenomes(emale_genes, emale_seqs) |>
#'   add_clusters(emale_cogs) +
#'   geom_seq() + geom_gene()
#' 
#' qq <- 
#' # link gene clusters with polygon
#' q0 + geom_link(aes(fill = cluster_id)) +
#' # link with curved polygons (bezier-like)
#' q0 + geom_link_curved(aes(fill = cluster_id)) +
#' # link gene clusters with lines
#' q0 + geom_link_line(aes(color = cluster_id))
#' 
#' qq + plot_layout(nrow = 1, guides = "collect")
geom_link <- function(
    mapping = NULL, data = links(), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    offset = 0.15, curve = NA, ...) {
  if (length(offset) == 1) offset <- offset[c(1, 1)]

  default_aes <- aes(y = .data$y, x = .data$x, xend = .data$xend,
    yend = .data$yend, xmin = .data$xmin, xmax = .data$xmax)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    geom = GeomLink, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, offset = offset, curve = curve, ...)
  )
}
#' @rdname geom_link
#' @return A ggplot2 layer with links.
#' @export
geom_link_curved <- function(
    mapping = NULL, data = links(), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    offset = 0.15, curve = 10, ...){
  
  geom_link(mapping = mapping, data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, na.rm = na.rm,
    offset = offset, curve = curve, ...)
}
#' @rdname geom_link
#' @return A ggplot2 layer with links.
#' @export
geom_link_line <- function(
    mapping = NULL, data = links(), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    ...) {
  default_aes <- aes(y = .data$y, yend = .data$yend,
    x = (.data$x + .data$xend) / 2, xend = (.data$xmin + .data$xmax) / 2)
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
    colour = "honeydew3", fill = "honeydew3", linewidth = 0.5, linetype = 1,
    alpha = 0.7
  ),
  required_aes = c("x", "xend", "y", "xmin", "xmax", "yend"),
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre",
      offset = c(0.15, 0.15), curve = NA) {
    if (TRUE) { # !coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "xend", "y", "xmin", "xmax", "yend")
      )
      polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
        poly <- link_to_poly(row$x, row$xend, row$y, row$xmin, row$xmax,
          row$yend, offset, curve)
        aes <- vctrs::data_frame(row[aesthetics])[rep(1, nrow(poly)), ]
        GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
      })

      ggplot2__ggname("link", do.call("grobTree", polys))
    }
  },
  draw_key = draw_key_polygon
)

link_to_poly <- function(x, xend, y, xmin, xmax, yend, offset, curve = NA) {
  if (y > yend) {
    y <- y - offset[1]
    yend <- yend + offset[2]
  } else {
    y <- y + offset[2]
    yend <- yend - offset[1]
  }

  if (is.na(curve) || curve == 0) {
    vctrs::data_frame(
      .name_repair = "minimal",
      y = c(y, yend, yend, y),
      x = c(xend, xmax, xmin, x)
    )
  } else {
    z1 <- sigmoid(xend, y, xmax, yend, curve)
    z2 <- sigmoid(xmin, yend, x, y, curve)
    vctrs::data_frame(
      .name_repair = "minimal",
      y = c(z1$y, z2$y),
      x = c(z1$x, z2$x)
    )
  }
}

sigmoid <- function(x1, y1, x2, y2, curve = 10, breaks = 100) {
  # parameter along the segment
  t <- seq(0, 1, length.out = breaks)
  # linear interpolation for y
  y <- y1 + t * (y2 - y1)

  # logistic “S” curve between 0 and 1
  s <- 1 / (1 + exp(-curve * (t - 0.5)))
  s <- (s - min(s)) / (max(s) - min(s))  # normalize 0–1

  # x follows sigmoid between endpoints
  x <- x1 + s * (x2 - x1)
  
  vctrs::data_frame(x=x, y=y)
}