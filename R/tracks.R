#' Named vector of track ids and types
#' @param x A gggenomes or gggenomes_layout object
#' @param track_type restrict to any combination of "seqs", "feats" and "links".
#' @export
track_ids <- function(x, ...){
  UseMethod("track_ids")
}

#' @export
track_ids.gggenomes <- function(x, ...){
  track_ids(x$data, ...)
}

#' @export
track_ids.gggenomes_layout <- function(x, track_type=c("seqs", "feats", "links")){
  track_type <- match_arg(track_type, several.ok = TRUE)
  ids <- flatten_chr(unname(map(track_type, ~names(x[[.x]]))))
  names(ids) <- track_types(x, track_type)
  ids
}

#' Basic info on tracks in a gggenomes object
#'
#' Call on a gggenomes or gggenomes_layout object to get a short tibble
#' with ids, types, index and size of loaded tracks.
#' @export
#' @inheritParams track_ids
track_info <- function(x, ...){
  UseMethod("track_info")
}

#' @export
track_info.gggenomes <- function(x, ...){
  track_info(x$data, ...)
}

#' @export
track_info.gggenomes_layout <- function(x, track_type = c("seqs", "feats", "links")){
  track_type <- match_arg(track_type, several.ok = TRUE)
  y <- tibble(
    id = track_ids(x, track_type),
    type = track_types(x, track_type),
    n = track_nrows(x, track_type)
  ) %>% group_by(type) %>%
  mutate(
    .after = type,
    i = row_number()
  )
  filter(y, type %in% track_type)
}

#' All types of all tracks
#' @inheritParams track_ids
#' @return a vector of all types of all selected tracks
track_types <- function(x, track_type = c("seqs", "feats", "links")){
  flatten_chr(unname(map(track_type, ~rep(.x, length(x[[.x]])))))
}

#' Number of rows of all tracks tables
#' @inheritParams track_ids
#' @return a vector of number of rows all selected tracks tables
track_nrows <- function(x, track_type = c("seqs", "feats", "links")){
  flatten_int(unname(map(track_type, ~map_int(x[[.x]], nrow))))
}

#' Track type by track id
#' @inheritParams track_ids
#' @return a character string with the track type
track_type <- function(x, track_id){
  track_ids <- track_ids(x)
  track_id <- vars_track(x, {{track_id}})
  names(track_ids)[track_ids == track_id]
}

## questionable
tracks <- function(x, ...){
  UseMethod("tracks")
}
tracks.gggenomes <- function(x, ...){
  tracks(x$data)
}
tracks.gggenomes_layout <- function(x, track_type = c("seqs", "feats", "links")){
  c(x$seqs, x$feats, x$links)
}


#' Convert a list of tibbles into tracks with magic
#' @keywords internal
as_tracks <- function(tracks, tracks_exprs, reserved_ids=NULL, context=NULL){
  # capture for df naming before first eval of tracks
  track_name <- as_label(enexpr(tracks))
  if(is.null(tracks)){
    return(tracks)
  }else if(is.data.frame(tracks)){
    tracks <- list(tracks)
    names(tracks) <- track_name
  }else if(is_character(tracks)){
    tracks <- list(read_context(tracks, context))
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
