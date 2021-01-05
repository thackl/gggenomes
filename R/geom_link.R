#' Draw links
#'
#' Note `.adjacent_only=TRUE` is somewhat redundant with
#' layout_links(adjacent_only =TRUE), which currently is always set to TRUE
#' @param offset distance between seq center and link start. Use two values
#'   `c(<offset_top>, <offset_bottom>)` for different top and bottom offsets
#' @export
geom_link <- function(mapping = NULL, data = links(), stat = "identity",
    position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    offset = 0.1, ...) {

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
  default_aes = aes(colour = "grey50", fill = "grey50", size = 0.5, linetype = 1,
    alpha = 0.7),

  required_aes = c("x", "xend", "y", "xmin", "xmax", "yend"),

  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre", offset = c(0.1, 0.1)) {
    if (TRUE){#!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "xend", "y", "xmin", "xmax", "yend")
      )
      polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
        poly <- link_to_poly(row$x, row$xend, row$y, row$xmin, row$xmax, row$yend, offset)
        aes <- ggplot2:::new_data_frame(row[aesthetics])[rep(1,5), ]
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

  ggplot2:::new_data_frame(list(
    y = c(y, y, yend, yend, y),
    x = c(x, xend, xmax, xmin, x)
  ))
}
