#' @export
track_ids <- function(x, ...){
  UseMethod("track_ids")
}

#' @export
track_ids.gggenomes <- function(x, ...){
  track_ids(x$data)
}

#' @export
track_ids.gggenomes_layout <- function(x, ...){
  track_ids <- c("seqs", names(x$features), names(x$links))
  names(track_ids) <- c("seqs", rep("features", length(x$features)),
                        rep("links", length(x$links)))
  track_ids
}

check_track_ids <- function(new_track_ids, old_track_ids, type, prefix){
  all_track_ids <- c(new_track_ids[new_track_ids != ""], old_track_ids)
  # track ids need to be unique
  if(any(duplicated(all_track_ids))){
    dups <- all_track_ids[duplicated(all_track_ids)]
    stop(paste("track ids need to be unique: ", dups))
  }

  no_names <- which(new_track_ids  == "")
  new_track_ids[no_names] <- paste0(prefix, no_names + sum(names(old_track_ids) == type))
  new_track_ids
}

#' @export
track <- function(x, ...){
  UseMethod("track")
}

track.gggenomes <- function(x, track_id){
  track(x$data, {{track_id}})
}

track.gggenomes_layout <- function(x, track_id){
  track_ids <- track_ids(x)
  track_id <-  tryCatch(
    tidyselect::vars_pull(track_ids, {{track_id}}),
    error = function(err){rlang::abort(paste("in get track()", err$message))})

  track_type <- names(track_ids)[track_ids == track_id]
  if(track_type == "seqs") track <- list(track_id)
  else  track <- list(track_type, track_id)
  pluck(x, !!! track)
}
