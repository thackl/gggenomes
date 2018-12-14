#' @export
add_contigs <- function(x, contigs, ...){
  UseMethod("add_contigs")
}
#' @export
add_contigs.gggenomes <- function(x, contigs, ...){
  x$data <- add_contigs(x$data, contigs, ...)
  x
}
#' @export
add_contigs.tbl_genome <- function(x, contigs, ...){
  x$contigs <- as_contigs(contigs, ...)
  # layout(x)
  x
}
#- links --------------------------------------------------------------------

#' @export
add_features <- function(x, features, track_id="features"){
  UseMethod("add_features")
}

#' @export
add_features.gggenomes <- function(x, features, track_id="features"){
  x$data <- add_features(x$data, features, track_id)
  x
}

#' @export
add_features.tbl_genome <- function(x, genes, track_id="genes"){
  x[[track_id]] <- as_features(genes, x$contigs)
  x
}

#- links --------------------------------------------------------------------

#' @export
add_genes <- function(x, genes, track_id="genes"){
  UseMethod("add_genes")
}

#' @export
add_genes.gggenomes <- function(x, genes, track_id="genes"){
  x$data <- add_genes(x$data, genes, track_id)
  x
}

#' @export
add_genes.tbl_genome <- function(x, genes, track_id="genes"){
  x[[track_id]] <- as_features(genes, x$contigs)
  x
}


#- links --------------------------------------------------------------------
#' @export
add_links <- function(x, links, track_id="links"){
  UseMethod("add_links")
}

#' @export
add_links.gggenomes <- function(x, links, track_id="links"){
  x$data <- add_links(x$data, links, track_id)
  x
}

#' @export
add_links.tbl_genome <- function(x, links, track_id="links"){
  x[[track_id]] <- as_links(links, x$contigs) # this is lossy, so
  x[[paste0(track_id, "_")]] <- links # also store orig links for re-layout
  x
}
