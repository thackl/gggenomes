#' Genome Layout
#'
#' @rdname tbl_genome_layout
#' @export
tbl_genome_layout <- function(chromosomes = NULL, features = list(), links = list(), ...) {
  as_tbl_genome_layout(list(chromosomes = chromosomes, links = links, features = features, ...))
}
#' @rdname tbl_genome_layout
#' @export
create_layout <- tbl_genome_layout
#' @export
as_tbl_genome_layout <- function(data, ...) {
  UseMethod('as_tbl_genome_layout')
}
#' Convert generic list
#'
#' @export
#' @importFrom purrr map
as_tbl_genome_layout.list <- function(data, ...){
  if(is.null(data$chromosomes)) stop("chromosomes required!")

  # layout chromosomes
  data$chromosomes <- as_tbl_chromosome_layout(data$chromosomes, ...)

  # layout features
  # if(is.data.frame(data$features)) data$features <- list(features = data$features)
  # data$features <- map(data$features, as_tbl_feature_layout, data$chromosomes)
  data$features <- as_tbl_feature_layout(data$features, data$chromosomes)
  # layout links
  # if(is.data.frame(data$links)) data$links <- list(links = data$links)
  # data$links <- map(data$links, as_tbl_link_layout, data$chromosomes)

  class(data) <- c('tbl_genome_layout')
  # active(data) <- "chromosomes"
  data
}
#' @rdname tbl_genome_layout
#' @export
is.tbl_genome_layout <- function(data) {
  inherits(data, 'tbl_genome_layout')
}
#' @export
#' @importFrom tibble as_tibble
as_tibble.tbl_genome_layout <- function(data, ...) {
  expose(data)
}
## as_tibble.tbl_genome_layout <- function(data, active = NULL, ...) {
##   if (is.null(active)) {
##     active <- attr(data, 'active')
##   }
##   if(is.null(data[[active]])) stop('Unknown data set ', active, call. = FALSE);
##   data[[active]]
##}
#' @export
as.data.frame.tbl_genome_layout <- function(x, ...) as.data.frame(expose(x))

#' @export
tibble::as_tibble

#' @export
expose <- function(data, what){
  UseMethod('expose', data)
}
#' @export
expose.tbl_genome_layout <- function(data, what=chromosomes) {
  what_string <- rlang::quo_text(rlang::enquo(what))
  if(is.null(data[[what_string]])) stop('Unknown data set ', what, call. = FALSE);
  data[[what_string]]
}
#' @param what data set to expose from tbl_genome_layout
#' @param ... filter arguments passed through to [dplyr::filter].
#' @export
data <- function(what=chromosomes, ...) {
  what_string <- rlang::quo_text(rlang::enquo(what))
  dots <- quos(...)
    function(data, ...){
        if(is.null(data[[what_string]])) stop('Unknown data set ', what, call. = FALSE);
        data[[what_string]] %>% filter(!!! dots)
    }
}
#' @export
expose_data <- data
#' `ggplot2::facet_null` checks data with `emtpy(df)` using `dim`. This causes
#' and error because dim(tbl_genome_layout) is undefined. Return dim of primary
#' table instead
#'
#' @export
dim.tbl_genome_layout <- function(x) dim(expose(x))
