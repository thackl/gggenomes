#' Pull out a specific track, possibly filtered
#'
#' Uses semantics similar to [dplyr::pull] to access tracks by unquoted ids or
#' positional arguments. Also supports filtering of the data.
#'
#' `pull_track()` works on all tracks at once, in the order seqs, features,
#' links.
#'
#' @param .x A gggenomes or gggenomes_layout object
#' @param .track_id The track to pull out, either as a literal variable name or
#' as a positive/negative integer giving the position from the left/right.
#' @param ... Logical predicates passed on to [dplyr::filter].
#' @export
pull_seqs <- function(.x, ...){
  UseMethod("pull_seqs")
}
#' @export
pull_seqs.gggenomes <- function(.x, ...){
  pull_seqs(.x$data, ...)
}
#' @export
pull_seqs.gggenomes_layout <- function(.x, ...){
  filter(.x$seqs, ...)
}
#' @rdname pull_seqs
#' @export
pull_features <- function(.x, .track_id=-1, ...){
  UseMethod("pull_features")
}
#' @export
pull_features.gggenomes <- function(.x, .track_id=-1, ...){
  pull_features(.x$data, {{.track_id}}, ...)
}
#' @export
pull_features.gggenomes_layout <- function(.x, .track_id=-1, ...){
  track_id <- tidyselect::vars_pull(names(.x$features), {{.track_id}})
  filter(.x$features[[track_id]], ...)
}
#' @rdname pull_seqs
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
  filter(.x$links[[.track_id]], ...)
}
#' @rdname pull_seqs
#' @export
pull_track <- function(.x, .track_id=1, ...){
  UseMethod("pull_track")
}
pull_track.gggenomes <- function(.x, .track_id=1, ...){
  pull_track(.x$data, {{.track_id}}, ...)
}
pull_track.gggenomes_layout <- function(.x, .track_id=1, ...){
  track_ids <- track_ids(.x)
  track_id <- tidyselect::vars_pull(track_ids, {{.track_id}})
  if(track_id == "seqs"){
    filter(.x$seqs, ...)
  }else{
    track_type <- names(track_ids)[track_ids == track_id]
    filter(.x[[track_type]][[track_id]], ...)
  }
}
