#' Draw feats
#'
#'@description
#' `geom_feat()` allows the user to draw (additional) features to the plot/graph. 
#' For example, specific regions within a sequence (e.g. transposons, introns, mutation hotspots) 
#' can be highlighted by color, size, etc.. 
#' 
#' @details
#' `geom_feat` uses `ggplot2::geom_segment` under the hood. As a result, 
#' different aesthetics such as *alpha*, *linewidth*, *color*, etc. 
#' can be called upon to modify the visualization of the data.
#' 
#' *By default, the function uses the first feature track.* 
#' 
#' @param data feat_layout: Uses first data frame stored in the `feats` track by default.
#' @param position describes how the position of different plotted features are adjusted. By default it uses `"pile"`, 
#' but different ggplot2 position adjustments, such as `"identity` or `"jitter"` can be used as well. 
#' @inheritParams ggplot2::geom_segment
#' @importFrom ggplot2 geom_segment
#' @export
#' @examples 
#' # Plotting data from the feats' track with adjusted linewidth and color
#' gggenomes(seqs = emale_seqs, feats = emale_ngaros) +
#' geom_seq() +
#' geom_feat(linewidth = 5, color = "darkred") 
#' 
#' # Geom_feat can be called several times as well, when specified what data should be used
#' gggenomes(seqs = emale_seqs, feats = list(emale_ngaros, emale_tirs)) +
#' geom_seq() +
#' geom_feat(linewidth = 5, color = "darkred") + #uses first feature track
#' geom_feat(data = feats(emale_tirs))
#' 
#' # Additional notes to feats can be added with functions such as: geom_feat_note / geom_feat_text
#' gggenomes(seqs = emale_seqs, feats = list(emale_ngaros, emale_tirs)) +
#' geom_seq() +
#' geom_feat(color = "darkred") +
#' geom_feat(data=feats(emale_tirs), color = "darkblue") +
#' geom_feat_note(data = feats(emale_ngaros), label="repeat region", size = 4) 
#' 
#' # Different position adjustments with a simple dataset 
#' exampledata <- tibble(
#' seq_id = c(rep("A", 3), rep("B", 3), rep("C", 3)),
#' start = c(0, 30, 15, 40, 80, 20, 30, 50, 70),
#' end = c(30, 90, 60, 60, 100, 80, 60, 90, 120))
#'
#' gggenomes(feats = exampledata) +
#' geom_feat(position = "identity", alpha = 0.5, linewidth = 0.5) +
#' geom_bin_label()
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
##                    arrow=arrow, size=0.5, color="grey85")))
##   }
##   r
## }

GeomFeat <- ggproto(
  "GeomFeat", GeomSegment,
  default_aes = aes(colour = "paleturquoise4", linewidth = 2, linetype = 1, alpha = NA)
)
