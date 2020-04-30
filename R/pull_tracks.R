#' Pull out a (filtered) feature track
#'
#' Similar semantics as dplyr::pull to access tracks by unquoted ids or
#' positional arguments. Also supports filtering of the data.
#'
#' @param .x A gggenomes or gggenomes_layout object
#' @param .track_id The track to pull out, either as a literal variable name or
#' as a positive/negative integer giving the position from the left/right.
#' @param ... Logical predicates passed on to dplyr::filter.
#' @export
pull_features <- function(.x, .track_id=1, ...){
  UseMethod("pull_features")
}
#' @export
pull_features.gggenomes <- function(.x, .track_id=1, ...){
  pull_features(.x$data, {{.track_id}}, ...)
}
#' @export
pull_features.gggenomes_layout <- function(.x, .track_id=1, ...){
  track_id <- tidyselect::vars_pull(names(.x$features), {{.track_id}})
  filter(.x$features[[track_id]], ...)
}

#' Pull out a (filtered) link track
#'
#' Similar semantics as dplyr::pull to access tracks by unquoted ids or
#' positional arguments. Also supports filtering of the data.
#'
#' @param .x A gggenomes or gggenomes_layout object
#' @param .track_id The track to pull out, either as a literal variable name or
#' as a positive/negative integer giving the position from the left/right.
#' @param ... Logical predicates passed on to dplyr::filter.
#' @export
pull_links <- function(.x, .track_id=1, ...){
  UseMethod("pull_links")
}
#' @export
pull_links.gggenomes <- function(.x, .track_id=1, ...){
  pull_links(.x$data, {{.track_id}}, ...)
}
#' @export
pull_links.gggenomes_layout <- function(.x, .track_id=1, ...){
  track_id <- tidyselect::vars_pull(names(.x$links), {{.track_id}})
  filter(.x$links[[track_id]], ...)
}
