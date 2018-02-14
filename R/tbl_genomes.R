#' tbl genomes
#'
#' @rdname tbl_genomes
#' @export
tbl_genomes <- function(contigs = NULL, links = NULL, features = NULL) {
  as_tbl_genomes(list(contigs = contigs, links = links, features = features))
}
#' @export
as_tbl_genomes <- function(x, ...) {
  UseMethod('as_tbl_genomes')
}
#' Convert generic list
#' 
#' @export
as_tbl_genomes.list <- function(x, ...){
    class(x) <- c('tbl_genomes')
    x
}
#' Potential Future Work
#' Convert Granges objects
#' 
#' @export
as_tbl_genomes.Granges <- function(x, ...) {
    stop('No support for ', class(x)[1], ' objects yet', call. = FALSE)
    tryCatch({
        as_tbl_genomes(x)
    }, error = function(e) stop('No support for ', class(x)[1], ' objects', call. = FALSE))
}
#' @export
#' @importFrom tibble as_tibble
as_tibble.tbl_genomes <- function(x, ...) expose(x)
#' @export
as.data.frame.tbl_genomes <- function(x, ...) expose(x)
#' @export
#' @importFrom ggplot2 fortify
fortify.tbl_genomes <- function(x, ...) expose(x)


#' expose with non-standard evaluation of `what` argument.
#'
#' @param data tbl_genomes
#' @param what tbl to expose
#' @export
expose <- function(data, what) UseMethod('expose', data)
#' @export
#' @importFrom rlang enquo quo_text
expose.tbl_genomes <- function(data, what=contigs) {
    what_string <- rlang::quo_text(rlang::enquo(what))
    if(is.null(data[[what_string]])) stop('Unknown data set ', what, call. = FALSE);
    data[[what_string]]
}
#' @export
#' @importFrom rlang enquo quo_text
expose_data <- function(what=contigs) {
    what_string <- rlang::quo_text(rlang::enquo(what))
    function(data, ...){
        if(is.null(data[[what_string]])) stop('Unknown data set ', what, call. = FALSE);
        data[[what_string]]
    }
}
