#' Re-layout a genome layout
#'
#' Re-layout the tracks and update the scales after seqs have been modified
#' @export
layout <- function(x, ...){
    UseMethod("layout")
}

#' @export
layout.gggenomes <- function(x, ignore_seqs=FALSE, ...){
  x$data <- layout(x$data, ...)
  x
}

#' @export
layout.gggenomes_layout <- function(x, ignore_seqs=FALSE, args_seqs = list(),
    args_feats = list(), args_links = list()){
  if(!ignore_seqs)
    x <- seq_seqs(x, exec(layout_seqs, get_seqs(x), !!!args_seqs,
                            !!!x$args_seqs))
   # note: tried this with map, but that somehow messes with !!!
   for(i in seq_along(x$feats))
     x$feats[[i]] %<>% exec(layout_feats, ., get_seqs(x), !!!args_feats)
   for(i in seq_along(x$links))
     x$links[[i]] <- exec(as_links, x$orig_links[[i]], get_seqs(x), !!!args_links)
  x
}

#' @export
drop_layout <- function(data, ...){
    UseMethod("drop_layout")
}
