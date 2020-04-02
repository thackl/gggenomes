#' @export
add_seqs <- function(x, seqs, ...){
  UseMethod("add_seqs")
}
#' @export
add_seqs.gggenomes <- function(x, seqs, ...){
  x$data <- add_seqs(x$data, seqs, ...)
  x
}
#' @export
add_seqs.gggenomes_layout <- function(x, seqs, ...){
  x$seqs <- as_seqs(seqs, ...)
  x
}

#- features --------------------------------------------------------------------
#' Add features
#' @param ... feature tables with names, i.e. genes=gene_df, snps=snp_df
#' @export
#'
#' @examples
#' gggenomes %>%
#'   add_features(genes=gene_df, snps=snp_df)
add_features <- function(x, ...){
  UseMethod("add_features")
}

#' @export
add_features.gggenomes <- function(x, ...){
  x$data <- add_features(x$data, ...)
  x
}

#' @export
add_features.gggenomes_layout <- function(x, ..., .auto_prefix="features"){
  tracks <- list(...)
  names(tracks) <- check_track_ids(names(tracks), track_ids(x), "features",
                                      .auto_prefix)
  # convert to feature layouts
  x$features <- c(x$features, map(tracks, as_features, x$seqs))
  x
}

#- links --------------------------------------------------------------------
#' @export
add_links <- function(x, ...){
  UseMethod("add_links")
}

#' @export
add_links.gggenomes <- function(x, ...){
  x$data <- add_links(x$data, ...)
  x
}

#' @export
add_links.gggenomes_layout <- function(x, ..., .auto_prefix="links"){
  tracks <- list(...)
  names(tracks) <- check_track_ids(names(tracks), track_ids(x), "links",
                                      .auto_prefix)
  # convert to layouts
  x$links <- c(x$links, map(tracks, as_links, x$seqs)) # this is lossy, so
  x$orig_links <- c(x$orig_links, tracks) # also store orig links for re-layout
  x
}


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

