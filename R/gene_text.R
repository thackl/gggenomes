#' @export
geom_gene_text <- function(mapping = NULL, data = genes(), stat="identity", position="identity",
                            angle = 45, hjust = 0, nudge_y = 0.1, size = 6, ...,
                           parse = FALSE, check_overlap = FALSE, na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE){

  default_aes <- aes(y=y,x=x,xend=xend,type=type,group=geom_id)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFeatText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      angle=angle,
      hjust=hjust,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
GeomFeatText <- ggproto("GeomFeatText", Geom,
  required_aes = c("x", "xend", "y", "label"),
  optional_as = c("type"),
  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2,
    xjust = 0.5
  ),

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    data <- coord$transform(data, panel_params)

    data$xx <- data$x * (1-data$xjust) + data$xend * (data$xjust)

    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$xx)
    }

    textGrob(
      lab,
      data$xx, data$y, default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  },

  draw_key = draw_key_text
)

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
