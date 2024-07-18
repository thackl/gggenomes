#' X-scale for genomic data
#'
#' `scale_x_bp()` is the default scale for genomic x-axis. It wraps
#' [ggplot2::scale_x_continuous()] using `label_bp()` as default labeller.
#' @inheritParams label_bp
#' @param ... Arguments passed on to [ggplot2::scale_x_continuous()]
#' @return A ggplot2 scale object with bp labels
#' @export
#' @examples
#' # scale_x_bp invoked by default
#' gggenomes(emale_genes) + geom_gene()
#'
#' # customize labels
#' gggenomes(emale_genes) + geom_gene() +
#'   scale_x_bp(suffix = "bp", sep = " ")
#'
#' # Note: xlim will overwrite scale_x_bp() with ggplot2::scale_x_continuous()
#' gggenomes(emale_genes) + geom_gene() +
#'   xlim(0, 3e4)
#'
#' # set limits explicitly with scale_x_bp() to avoid overwrite
#' gggenomes(emale_genes) + geom_gene() +
#'   scale_x_bp(limits = c(0, 3e4))
scale_x_bp <- function(..., suffix = "", sep = "", accuracy = 1) {
  ggplot2::scale_x_continuous(..., labels = label_bp(
    suffix = suffix, sep = sep,
    accuracy = accuracy
  ))
}

# gggenomes::scale_x_continuous is added as default scale to plot if no scale is
# added explicitly - make this default to scale_x_bp().
#
# NOTE: do not export - overwrites ggplot::scale_x_continuous for other plots
scale_x_continuous <- function(...) {
  scale_x_bp(...)
}

#' @inheritParams scales::label_bytes
#' @param suffix unit suffix e.g. "bp"
#' @param sep between number and unit prefix+suffix
#' @return A labeller function for genomic data
#' @export
#' @rdname scale_x_bp
label_bp <- function(suffix = "", sep = "", accuracy = 1) {
  scales__force_all(accuracy)
  function(x) {
    breaks <- c(0, 10^c(k = 3, M = 6, G = 9))
    n_suffix <- cut(abs(x),
      breaks = c(unname(breaks), Inf),
      labels = c(names(breaks)), right = FALSE
    )
    n_suffix[is.na(n_suffix)] <- ""
    suffix <- paste0(sep, n_suffix, suffix)
    scale <- 1 / breaks[n_suffix]
    scale[which(scale %in% c(Inf, NA))] <- 1
    scales::number(x,
      accuracy = accuracy, scale = unname(scale),
      suffix = suffix
    )
  }
}
