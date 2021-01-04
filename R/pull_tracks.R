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
#' @param .ignore a regular string vector listing track ids to ignore.
#' @param ... Logical predicates passed on to [dplyr::filter].
#' @export
pull_track <- function(.x, .track_id=1, ..., .types = c("seqs", "features",
    "links"), .ignore=NA){
  UseMethod("pull_track")
}
#' @export
pull_track.gggenomes <- function(.x, .track_id=1, ..., .types = c("seqs",
    "features", "links"), .ignore=NA){
  pull_track(.x$data, {{.track_id}}, ..., .types=.types, .ignore=.ignore)
}
#' @export
pull_track.gggenomes_layout <- function(.x, .track_id=1, ..., .types = c("seqs",
    "features", "links"), .ignore=NA){
  track_id <- vars_track(.x, {{.track_id}}, .types, .ignore)
  filter(.x[[track_type(.x, track_id)]][[track_id]], ...)
}

#' @rdname pull_track
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
  filter(seqs(.x), ...)
}
#' @rdname pull_track
#' @export
pull_features <- function(.x, .track_id=1, ..., .ignore="genes"){
  UseMethod("pull_features")
}
#' @export
pull_features.gggenomes <- function(.x, .track_id=1, ..., .ignore="genes"){
  pull_features(.x$data, {{.track_id}}, ..., .ignore=.ignore)
}
#' @export
pull_features.gggenomes_layout <- function(.x, .track_id=1, ..., .ignore="genes"){
  track_ids <- setdiff(names(.x$features), .ignore)
  track_id <- tidyselect::vars_pull(track_ids, {{.track_id}})
  filter(.x$features[[track_id]], ...)
}
#' @rdname pull_track
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

#' Tidyselect track variables
#'
#' Based on `tidyselect::vars_pull`. Powers track selection in `pull_*/use*`.
#' Catches and modifies errors from vars_pull to track-relevant info.
#' @param x A gggenomes or gggenomes_layout object
#' @param track_id as quoted or unquoted name or as positive/negative integer
#'   giving the position from the left/right.
#' @param types restrict to these types of tracks - affects position-based
#'   selection
#' @return The selected track_id as an unnamed string
vars_track <- function(x, track_id, types = c("seqs", "features", "links"),
    ignore = NA){
  types <- match.arg(types, several.ok = T)
  track_ids <- track_ids(x, types)
  track_ids <- setdiff(track_ids, ignore)
  tryCatch(tidyselect::vars_pull(track_ids, {{track_id}}),
    # this code has to be here - calling `error=fun_defined_elsewhere` causes
    # issue with accessing local variables
    error = function(cnd){
      if(inherits(cnd,"vctrs_error_subscript")){
        class(cnd) <- c("rlang_error", "error", "condition")
        cnd$message <- vars_track_error(cnd$i, track_ids, ignore)
      }else{
        m <- str_match(cnd$message, "object (.*) not found")
        if(!is.na(m[1,1])){
          cnd$message <- vars_track_error(m[1,2], track_ids, ignore)
        }
      }
      stop(cnd)
    })
}

#' Error messages for `vars_track``
vars_track_error <- function(bad_value, track_ids, ignore){
  if(is_function(bad_value)) bad_value <- "<function>"
  paste("track", as_name(bad_value), "doesn't exist", "\nAvailable tracks:",
        paste(track_ids, collapse=", "),  paste0("(", length(track_ids), ")"),
        "\nIgnored tracks:", paste(ignore, collapse=", "))
}
