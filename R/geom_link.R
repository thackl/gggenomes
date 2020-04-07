geom_link2 <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomLink, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomLink <- ggproto(
  "GeomLink", Geom,
  default_aes = aes(colour = "black", fill = "grey35", size = 0.5, linetype = 1,
    alpha = NA),

  required_aes = c("x", "xend", "y", "x2", "xend2", "y2"),

  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
    if (TRUE){#!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "xend", "y", "x2", "xend2", "y2")
      )

      polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
        poly <- link_to_poly(row$x, row$xend, row$y, row$x2, row$xend2, row$y2)
        aes <- ggplot2:::new_data_frame(row[aesthetics])[rep(1,5), ]

        GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
      })

      ggplot2:::ggname("link", do.call("grobTree", polys))
    }
  },
  draw_key = draw_key_polygon
)

link_to_poly <- function(x, xend, y, x2, xend2, y2) {
  ggplot2:::new_data_frame(list(
    y = c(y, y, y2, y2, y),
    x = c(x, xend, xend2, x2, x)
  ))
}
