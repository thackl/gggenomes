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
    args_features = list(), args_links = list()){
  if(!ignore_seqs)
    x$seqs <- exec(layout_seqs, x$seqs, !!!args_seqs, !!!x$args_seqs)
  # note: tried this with map, but that somehow messes with !!!
  for(i in seq_along(x$features))
    x$features[[i]] %<>% exec(layout_features, ., x$seqs, !!!args_features)
  for(i in seq_along(x$links))
    x$links[[i]] <- exec(as_links, x$orig_links[[i]], x$seqs, !!!args_links)
  x
}

#' @export
drop_layout <- function(data, ...){
    UseMethod("drop_layout")
}

#' @export
seqs <- function(x){
  UseMethod("seqs")
}

#' @export
`seqs<-` <- function(x, value){
  UseMethod("seqs<-")
}

#' @export
seqs.gggenomes <- function(x){
  seqs(x$data)
}

#' @export
seqs.gggenomes_layout <- function(x){
  x$seqs
}

#' @export
`seqs<-.gggenomes` <- function(x, value) {
  seqs(x$data) <- value
  x
}

#' @export
`seqs<-.gggenomes_layout` <- function(x, value) {
  x$seqs <- value
  x
}
