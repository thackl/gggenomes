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

as_tracks <- function(tracks, tracks_exprs, reserved_ids=NULL){
  # capture for df naming before first eval of tracks
  track_name <- as_label(enexpr(tracks))

  if(is.null(tracks)){
    return(tracks)
  }else if(is.data.frame(tracks)){
    tracks <- list(tracks)
    names(tracks) <- track_name
  }else if(is.list(tracks)){
    if(!is.list(tracks_exprs)) # make sure tracks_exprs is a list
      tracks_exprs <- as.list(tracks_exprs)[-1L]
    if(length(tracks_exprs) != length(tracks)){
      names(tracks) <- name_unnamed_from_template(tracks, prefix="track", offset=length(reserved_ids))
    }else{
      names(tracks) <- name_unnamed_from_values(tracks_exprs)
    }
    for(n in names(tracks)){
      if(!is.data.frame(tracks[[n]]))
        abort(paste0("track '", n, "' needs to be a data frame"))
    }
  }else{
    abort(paste0("track '", track_name, "' needs to be a data frame or a list of data frames"))
  }

  # track ids need to be unique
  all_track_ids <- c(names(tracks), reserved_ids)
  if(any(duplicated(all_track_ids))){
    dups <- all_track_ids[duplicated(all_track_ids)]
    abort(paste("track ids need to be unique: ", dups))
  }

  tracks
}

name_unnamed_from_template <- function(x, prefix, offset=0){
  unnamed <- !have_name(x)
  names(x)[unnamed] <- paste0(prefix, which(unnamed) + offset -1)
  names(x)
}

name_unnamed_from_values <- function(x){
  unnamed <- !have_name(x)
  names(x)[unnamed] <- x[unnamed]
  names(x)
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
