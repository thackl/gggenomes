#' Re-layout a genome layout
#'
#' Re-layout the tracks and update the scales after seqs have been modified
#' @export
layout <- function(x, ...){
    UseMethod("layout")
}

#' @export
layout.gggenomes <- function(x, ignore_seqs=FALSE){
  x$data <- layout(x$data)
  ## scale_name <- x$data[["ggargs_"]]$scale_name
  ## scale_args <- x$data[["ggargs_"]]$scale_args
  ## scale_args$data <- x$data
  ## x <- x + do.call(paste0("scale_gggenomes_", scale_name), scale_args)
  x
}

#' @export
layout.gggenomes_layout <- function(x, ignore_seqs=FALSE){
  if(!ignore_seqs) x$seqs <- exec(layout_seqs, x$seqs, !!!x$seqs_params)
  x$features %<>% map(layout_features, x$seqs)
  x$links <- map(x$orig_links, as_links, x$seqs)
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
