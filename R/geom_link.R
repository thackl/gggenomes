#' Draw links
#' 
#' @description
#' `geom_link()` allows the user to link loci/regions between two sequences/genomes with one another.
#'    
#' *Note that by default only links between adjacent sequences are computed and shown.* 
#' *To compute and show all links between all genomes, set `gggenomes(..., adjacent_only=FALSE)`.*
#' 
#' @details
#' The function calls upon the data stored within the `link` track. Data frames added to
#' this track have `seq_id` and `seq_id2` as required variables. Optional and recommended variables include 
#' `start`, `start2`, `end`, `end2`, `bin_id`, `bin_id2` and `strand`.     
#'    
#' *Keep in mind: when start/end is not specified, links will be created between the entire contigs of `seq_id` and `seq_id2`*
#' @param offset distance between seq center and link start. Use two values
#'   `c(<offset_top>, <offset_bottom>)` for different top and bottom offsets
#' @export
#' @examples
#' p <- gggenomes(seqs=emale_seqs, links = emale_ava) + geom_seq()
#' p + geom_link()
#' 
#' # change offset from seqs
#' p + geom_link(aes(fill=de, color=de), offset = 0.05) +
#'   scale_fill_viridis_b() + scale_colour_viridis_b()
#'   
#' # combine with flip
#' p %>% flip(3,4,5) + geom_link()
#' # compute & show all links among all genomes (not recommended for large dataset)
#' gggenomes(links=emale_ava, adjacent_only = FALSE) + geom_link()
geom_link <- function(mapping = NULL, data = links(), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    offset = 0.15, ...) {

  if(length(offset) == 1) offset <- offset[c(1,1)]

  default_aes <- aes(y=y,x=x,xend=xend,yend=yend,xmin=xmin,xmax=xmax)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    geom = GeomLink, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, offset = offset, ...)
  )
}

GeomLink <- ggproto(
  "GeomLink", Geom,
  default_aes = aes(colour = "honeydew3", fill = "honeydew3", size = 0.5, linetype = 1,
    alpha = 0.7),

  required_aes = c("x", "xend", "y", "xmin", "xmax", "yend"),

  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre", offset = c(0.15, 0.15)) {
    if (TRUE){#!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "xend", "y", "xmin", "xmax", "yend")
      )
      polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
        poly <- link_to_poly(row$x, row$xend, row$y, row$xmin, row$xmax, row$yend, offset)
        aes <- vctrs::data_frame(row[aesthetics])[rep(1,5), ]
        GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
      })

      ggplot2:::ggname("link", do.call("grobTree", polys))
    }
  },
  draw_key = draw_key_polygon
)

link_to_poly <- function(x, xend, y, xmin, xmax, yend, offset) {
  if(y > yend){
    y <- y - offset[1]
    yend <- yend + offset[2]
  }else{
    y <- y + offset[2]
    yend <- yend - offset[1]
  }

  vctrs::data_frame(
    .name_repair = "minimal",
    y = c(y, y, yend, yend, y),
    x = c(x, xend, xmax, xmin, x)
  )
}
