#' Genome Layout
#'
#' @rdname tbl_genome
#' @export
as_genomes <- function(contigs, genes = NULL, links = NULL, ...) {

  contigs %<>% as_contigs
  if(!is.null(genes)) genes %<>% as_features(contigs)
  ll <- list(NULL, NULL)
  if(!is.null(links)) ll <- as_links(links, contigs)

  x <- list(contigs=contigs, genes=genes, links=ll[[1]], links_orig=ll[[2]])
  set_class(x, "tbl_genome", "prepend")
}

#' `ggplot2::facet_null` checks data with `empty(df)` using `dim`. This causes
#' and error because dim(tbl_genome_layout) is undefined. Return dim of primary
#' table instead
#'
#' @export
dim.tbl_genome <- function(x) dim(expose(x))

#' Expose a single tibble of a tbl_genome object
#'
#' A tbl_genome is actually a list of tibbles. Expose lets you access a
#' particular tibble within the list.
#' 
#' @export
#' @param what data set to expose from tbl_genome_layout
expose <- function(data, what){
  UseMethod('expose', data)
}
#' @export
expose.tbl_genome <- function(data, what=contigs) {
  what_string <- rlang::quo_text(rlang::enquo(what))
  if(is.null(data[[what_string]])) stop('Unknown data set ', what, call. = FALSE);
  data[[what_string]]
}

#' Use a (filtered) tibble inside a `geom_*` call.
#' 
#' @inheritParams expose
#' @param ... filter arguments passed through to [dplyr::filter].
#' @export
use <- function(what=contigs, ...) {
  what_string <- rlang::quo_text(rlang::enquo(what))
  dots <- quos(...)
    function(data, ...){
        if(is.null(data[[what_string]])) stop('Unknown data set ', what, call. = FALSE);
        data[[what_string]] %>% filter(!!! dots)
    }
}
