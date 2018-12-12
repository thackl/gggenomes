#' activate
#'
#' @export
activate <- function(data, what) {
  UseMethod('activate')
}
#' @export
#' @importFrom rlang enquo quo_text
activate.tbl_genome_layout <- function(data, what) {
    active(data) <- quo_text(enquo(what))
    data
}

#' @rdname activate
#' @export
active <- function(data) {
    attr(data, 'active')
}
`active<-` <- function(data, value) {
    value <- gsub('"', '', value)
    if(is.null(data[[value]])) stop('Unknown data set ', value, call. = FALSE);
    attr(data, 'active') <- value
    data
}
