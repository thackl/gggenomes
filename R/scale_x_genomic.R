#' Genomic scales and base-pair labels
#'
#' Format genomic positions and scales using human-readable base-pair units.
#' `scale_x_genomic()` is the default genomic x-axis scale for gggenomes plots.
#' It wraps [ggplot2::scale_x_continuous()] with [guide_scalebar()] as its
#' default guide. See [axis_scalebar()] for convenient configuration and styling
#' of the scalebar. `format_bp()` formats numbers as base-pair quantities, and
#' powers `label_bp()`, the default `scale_x_genomic()` labelling function.
#' @param ... Arguments passed on to [ggplot2::scale_x_continuous()]
#' @return A ggplot2 scale object with bp labels
#' @inheritParams ggplot2::scale_x_continuous
#' @inheritParams label_bp
#' @export
#' @examples
#' library(patchwork)
#'
#' p0 <- gggenomes(genes = emale_genes) |> pick(1:2) + geom_gene()
#' p1 <- p0 + scale_x_genomic("base pairs", guide = "axis", unit = "", sep = "")
#' p2 <- p1 + scale_x_genomic(limits=c(-1000, 5000)) + theme_minimal()
#' p3 <- p0 + scale_x_continuous("regular x-axis")
#' p0 + p1 + p2 + p3 + plot_layout(ncol=2)
scale_x_genomic <- function(..., guide = "scalebar", unit = "bp", sep = " ",
    digits = 3, labels = NULL) {
  labels <- labels %||% label_bp(unit = unit, sep = sep, digits = digits)

  ggplot2::scale_x_continuous(..., guide = guide, labels = labels)
}

#' @describeIn scale_x_genomic format a number as human-readable base-pair value.
#' @inheritParams base::format
#' @param x numeric base-pair value.
#' @param unit unit suffix
#' @param sep separator between number and unit prefix+suffix
#' @param prefixes SI prefixes per thousands.
#' @return Character scalar.
#' @export
#' @examples
#' format_bp(c(0, 5e5, 1e6, 1.5e6, 2e6), digits=3, scientific=TRUE)
format_bp <- function(x, unit = "bp", sep = " ", digits = 3, 
    prefixes = c("k" = 1e3, "M" = 1e6, "G" = 1e9), trim = TRUE,
    scientific = FALSE, ...) {
 
  i <- findInterval(max(abs(x), na.rm=TRUE), prefixes)
  prefix <- ""
 
  if(i > 0){
    x <- x / prefixes[i]
    prefix <- names(prefixes)[i]
  }

  x <- format(x, trim = trim, scientific = scientific, digits = digits, ...)

  paste0(x, sep, prefix, unit)
}

#' @return A labeller function for genomic data
#' @export
#' @rdname scale_x_genomic
label_bp <- function(unit = "bp", sep = " ", digits = 3) {
  \(x) {
    format_bp(x, unit = unit, sep = sep, digits = digits)
  }
}