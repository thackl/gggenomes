#' Re-layout a genome layout
#'
#' Re-layout the tracks and update the scales after seqs have been modified
#' @param x layout
#' @param ... additional data
#' @return layout with updated scales
#' @export
layout <- function(x, ...) {
  UseMethod("layout")
}

#' @export
layout.gggenomes <- function(x, ignore_seqs = FALSE, ...) {
  x$data <- layout(x$data, ...)
  x
}

#' @export
layout.gggenomes_layout <- function(
    x, ignore_seqs = FALSE, args_seqs = list(),
    args_feats = list(), args_links = list(), ...) {

  # overwrite saved args in layout with current params
  args_seqs <- utils::modifyList(x$args_seqs, args_seqs)
  args_feats <- utils::modifyList(x$args_feats, args_feats)
  args_links <- utils::modifyList(x$args_links, args_links)

  if (!ignore_seqs) {
    x <- set_seqs(x, exec(layout_seqs, get_seqs(x), !!!args_seqs))
  }
  # note: tried this with map, but that somehow messes with !!!
  for (i in seq_along(x$feats)) {
    x$feats[[i]] <- exec(layout_feats, x$feats[[i]], get_seqs(x), !!!args_feats)
  }
  for (i in seq_along(x$links)) {
    x$links[[i]] <- exec(as_links, x$orig_links[[i]], get_seqs(x), !!!args_links)
  }
  x
}

#' Drop a genome layout
#'
#' @param data layout
#' @param ... additional data
#' @return gggenomes object without layout
#' @export
drop_layout <- function(data, ...) {
  UseMethod("drop_layout")
}

check_layout_outside <- function(x, y) {
  if (nrow(x) > nrow(y)){
    rlang::inform(
      message = function(...) {
        cli::format_message(c(
          "i" = "Items outside plot detected",
          paste(
            "Some of your feats, genes or links are not plotted because they",
            "fall outside your given sequence set. This is expected if you",
            "zoomed in or picked a subset of sequences. But it could also",
            "indicate a data mismatch. So we show this note once in a while.",
            "Examples of dropped items are:",
            sep = " "),
          # put this inside inform, so it's only computed if message is
          # actually needed
          get_missing(x, y, n=3, cols = 1:7)
        ))
      },
      .frequency = "regularly",
      .frequency_id = "gggenomes-layout-outside"
    )
  }
}


check_layout_marginal <- function(x) {

  if (sum(x[[".marginal"]], x[[".marginal2"]])){
    rlang::inform(
      message = c(
        "i" = "Marginal items detected",
        paste(
          "Some of your feats, genes or links extend across the edges of your",
          "specified sequence loci and into the margins. By default, these",
          "marginal items are dropped. Adjust with:",
          sep = " "),
        "*" = "`gggenomes(marginal = c('drop', 'keep', 'trim'))`",
        "*" = "`focus(.marginal = c('drop', 'keep', 'trim'))``.",
        "See `vignette('marginal', package = 'gggenomes')` for details."
      ),
      .frequency = "regularly",
      .frequency_id = "gggenomes-layout-marginal"
    )
  }
}



get_missing <- function(x, y, n=5, cols=1:7) {
  missing <- x |>
    anti_join(y, by = intersect(names(x), names(y))) |>
    slice_head(n = n) |> select(cols)

  msg <- utils::capture.output(print(missing, n = 5, width = Inf))
  msg[-1:-3]
}