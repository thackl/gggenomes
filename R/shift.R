#' Shift bins left/right
#'
#' Shift bins along the x-axis, i.e. left or right in the default plot
#' layout. This is useful to align feats of interest in different bins.
#'
#' @param x gggenomes object
#' @param bins to shift left/right, select-like expression
#' @param by shift each bin by this many bases. Single value or vector of the
#' same length as bins.
#' @param center horizontal centering
#' @return gggenomes object with shifted seqs
#' @export
#' @examples
#' p0 <- gggenomes(emale_genes, emale_seqs) +
#'   geom_seq() + geom_gene()
#'
#' # Slide one bin left and one bin right
#' p1 <- p0 |> shift(2:3, by = c(-8000, 10000))
#'
#' # align all bins to a target gene
#' mcp <- emale_genes |>
#'   dplyr::filter(name == "MCP") |>
#'   dplyr::group_by(seq_id) |>
#'   dplyr::slice_head(n = 1) # some have fragmented MCP gene, keep only first
#'
#' p2 <- p0 |> shift(all_of(mcp$seq_id), by = -mcp$start) +
#'   geom_gene(data = genes(name == "MCP"), fill = "#01b9af")
#'
#' library(patchwork)
#' p0 + p1 + p2
shift <- function(x, bins = everything(), by = 0, center = FALSE) {
  # split by bin_id and select bins
  s <- get_seqs(x)
  l <- s %>% split_by(.data$bin_id)
  i <- tidyselect::eval_select(expr({{ bins }}), l)
  if (length(i) == 0) rlang::abort("no bins selected")
  if (length(by) > 1 & length(by) != length(i)) {
    rlang::abort(paste("by needs to be a single value or a vector of the same length as bins:", length(i)))
  } else if (length(by) == 1) {
    by <- rep(by, length(i))
  }

  for (k in seq_along(i)) {
    j <- i[k]
    if (center) {
      l[[j]]$bin_offset <- l[[j]]$bin_offset - (max(l[[j]]$xend) / 2)
    }
    if (by[k] != 0) {
      l[[j]]$bin_offset <- l[[j]]$bin_offset + by[k]
    }
  }

  x <- set_seqs(x, bind_rows(l))
  layout(x, args_seqs = list(keep = c("strand", "bin_offset")))
}
