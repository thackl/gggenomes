#' Genome Layout
#'
#' @rdname tbl_genome
#' @export
as_genomes <- function(contigs, genes = NULL, links = NULL, ...) {
  x <- list()
  x %<>% set_class("tbl_genome", "prepend")

  x %<>% add_contigs(contigs)
  if(!is.null(genes)) x %<>% add_genes(genes)
  if(!is.null(links)) x %<>% add_links(links)
  x
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
