#' Draw feats
#'
#' @param data feat_layout
#' @export
geom_feat <- function(mapping = NULL, data = feats(), stat="identity",
    position = "pile", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
    ...) {
  # TODO: arrow tip
  default_aes <- aes(x, y, xend=xend, yend=y)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    geom = GeomFeat, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

## deprecatedx
## geom_feat <- function(mapping = NULL, data = feats(),
##     arrow = NULL, nudge_by_strand = NULL, ...){

##   mapping <- aes_nudge_by_strand(mapping, nudge_by_strand)

##   # TODO: would be cleaner with GeomFeat ggproto...
##   if (has_name(mapping, "size")) aes_intersect(mapping, aes(size = 3))
##   if (any(has_name(mapping, c("color", "colour")))) aes_intersect(mapping, aes(color = "grey40"))
##   r <- list(geom_segment(mapping = mapping, data = data, ...))

##   if (!rlang::is_null(arrow)){
##     if(!inherits(arrow, "arrow")) arrow <- grid::arrow(length = unit(2, "mm"))
##     r <- c(r, list(
##       geom_segment(aes(x=ifelse(x<xend, xend-1, xend+1), y, xend=xend, yend=y), data=data,
##                    arrow=arrow, linewidth=0.5, color="grey85")))
##   }
##   r
## }

GeomFeat <- ggproto(
  "GeomFeat", GeomSegment,
  default_aes = aes(colour = "paleturquoise4", linewidth = 2, linetype = 1, alpha = NA)
)
