#' Draw wiggle ribbons or lines
#'
#' Visualize data that varies along sequences as ribbons, lines, lineranges, 
#' etc.                 
#'  
#' Geom_wiggle plots the wiggle data in both directions around the median. 
#' Geom_coverage plots the data only in positive direction. 
#' Both functions use data from the feats' track.
#'
#' @inheritParams ggplot2::geom_ribbon
#' @param bounds geom_wiggle uses mid, low and high boundary values for plotting wiggle data. Can
#'   be both a function or a vector returning those three values. Defaults to
#'   [Hmisc::smedian.hilow].
#' @param max geom_coverage uses the function [base::max] by default, which plots data in positive direction. 
#'   ([base::min] Can also be called here when the input data )
#' @param height distance in plot between lowest and highest point of the wiggle data.
#' @param offset distance between seq center and wiggle mid/start.
#' @export
#' @examples 
#' # Plotting data with geom_coverage with increased height.
#' gggenomes(seqs=emale_seqs, feats=emale_gc) +
#' geom_coverage(aes(z = score), height = 0.5) +
#' geom_seq()
#'
#' # In opposite direction by calling base::min and taking the negative values of "score"
#' gggenomes(seqs=emale_seqs, feats=emale_gc) +
#' geom_coverage(aes(z = -score), max = base::min, height = 0.5) +
#' geom_seq()
#' 
#' # GC-content plotted as points with variable color in geom_coverage
#' gggenomes(seqs=emale_seqs, feats=emale_gc) +
#' geom_coverage(aes(z = score, color = score), height = 0.5, geom = "point") +
#' geom_seq()
#' @rdname geom_wiggle
geom_coverage <- function(mapping = NULL, data = feats(), stat="coverage",
                          geom="ribbon", position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, offset = 0, height = .2,
                          max = base::max, ...) {
    default_aes <- aes(x=(x+xend)/2, y=y, group=seq_id)
    mapping <- aes_intersect(mapping, default_aes)
    
    layer(
      geom = geom, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, offset=offset, height=height, bounds=max, ...)
    )
  }

StatCoverage <- ggproto("StatCoverage", Stat,
                      setup_params = function(data, params) {
                        # make sure this is a function even if a vector was supplied
                        bf <- as_bounds(params$bounds)
                        if (environmentName(environment(bf)) == "Hmisc" && !requireNamespace("Hmisc", quietly=TRUE))
                          abort("Hmisc package required for default wiggle bounds. Overwrite with custom bounds or bounds-function")
                        bs <- bf(data$z)
                        
                        if(length(bs) != 1) abort("Max need to return exactly one number")
                        inform(c("coverage max", paste(c("max:"), unname(bs))))
                        
                        params$rescale <- params$height / abs(bs)
                        params
                      },
                      compute_group = function(data, scales,height=NA, bounds=NA, offset=0, mid=NA, rescale=NA){
                        data$ymin <- data$y + offset
                        data$y <- data$z * rescale + data$ymin
                        data$ymax <- data$y
                        data
                      },
                      required_aes = c("x", "y", "z")
)

as_bounds <- function(.f, ...) {
  UseMethod("as_bounds")
}
as_bounds.default <- purrr:::as_mapper.default
as_bounds.numeric <- function(.f){
  function(...) .f
}

