#' Draw wiggle ribbons or lines
#'
#' @param bounds geom_wiggle uses mid, low and high boundary values for plotting wiggle data. Can
#'   be both a function or a vector returning those three values. Defaults to
#'   [Hmisc::smedian.hilow].
#' @param height distance in plot between lowest and highest point of the wiggle data.
#' @param offset distance between seq center and wiggle mid/start.
#' @section Aesthetics: `geom_wiggle()` and `geom_coverage()` understand aesthetics depending on the
#'   chosen underlying ggplot geom, by default [ggplot2::geom_ribbon()]. Other
#'   options that play well are for example [ggplot2::geom_line()],
#'   [ggplot2::geom_linerange()], [ggplot2::geom_point()]. The only required
#'   aesthetic is:
#'   * **z**
#' @export
#' @examples
#' 
#' # wiggle's default bounds function requires Hmisc
#' if (requireNamespace("Hmisc", quietly = TRUE)) {
#' 
#' # Plot varying GC-content along sequences as ribbon
#' gggenomes(seqs = emale_seqs, feats = emale_gc) +
#'   geom_wiggle(aes(z = score)) +
#'   geom_seq()
#'
#' # customize color and position
#' gggenomes(genes = emale_genes, seqs = emale_seqs, feats = emale_gc) +
#'   geom_wiggle(aes(z = score), fill = "lavenderblush3", offset = -.3, height = .5) +
#'   geom_seq() + geom_gene()
#'
#' # GC-content as line and with variable color
#' gggenomes(seqs = emale_seqs, feats = emale_gc) +
#'   geom_wiggle(aes(z = score, color = score), geom = "line", bounds = c(.5, 0, 1)) +
#'   geom_seq() +
#'   scale_colour_viridis_b(option = "A")
#'
#' # or as lineranges
#' gggenomes(seqs = emale_seqs, feats = emale_gc) +
#'   geom_wiggle(aes(z = score, color = score), geom = "linerange") +
#'   geom_seq() +
#'   scale_colour_viridis_b(option = "A")
#' 
#' }
geom_wiggle <- function(
    mapping = NULL, data = feats(), stat = "wiggle",
    geom = "ribbon", position = "identity", na.rm = FALSE, show.legend = NA,
    inherit.aes = TRUE, offset = 0, height = .8,
    bounds = Hmisc::smedian.hilow, ...) {
  default_aes <- aes(x = (.data$x + .data$xend) / 2, y = .data$y, group = .data$seq_id)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    geom = geom, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, offset = offset, height = height, bounds = bounds, ...)
  )
}

StatWiggle <- ggproto("StatWiggle", Stat,
  setup_params = function(data, params) {
    # make sure this is a function even if a vector was supplied
    bf <- as_bounds(params$bounds)
    if (environmentName(environment(bf)) == "Hmisc" && !requireNamespace("Hmisc", quietly = TRUE)) {
      abort("Hmisc package required for default wiggle bounds. Overwrite with custom bounds or bounds-function")
    }
    bs <- bf(data$z)

    if (length(bs) != 3) abort("Bounds need to return exactly three numbers: mid, low, high")
    inform(c("wiggle bounds", paste(c("mid: ", "low: ", "high:"), unname(bs))))

    params$rescale <- params$height / diff(bs[2:3])
    params$mid <- bs[1] * params$rescale
    params
  },
  compute_group = function(data, scales, height = NA, bounds = NA, offset = 0, mid = NA, rescale = NA) {
    data$ymin <- data$y + offset
    data$y <- data$z * rescale - mid + data$ymin
    data$ymax <- data$y
    data
  },
  required_aes = c("x", "y", "z")
)

as_bounds <- function(.f, ...) {
  UseMethod("as_bounds")
}
#' @export
as_bounds.default <- purrr__as_mapper.default
#' @export
as_bounds.numeric <- function(.f, ...) {
  function(...) .f
}
