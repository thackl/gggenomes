#' Add gene clusters
#' @export
add_clusters <- function(x, ..., .track_id = "genes"){
  UseMethod("add_clusters")
}

#' @export
add_clusters.gggenomes <- function(x, ..., .track_id = "genes"){
  x$data <- add_clusters(x$data, ..., .track_id = {{ .track_id }})
  x
}

#' @export
add_clusters.gggenomes_layout <- function(x, ..., .track_id = "genes"){
  if(!has_dots())
    rlang::abort("no clusters data provided - did you forget parent_track_id as first argument")

  pid <- tidyselect::vars_pull(track_ids(x), {{.track_id}})
  dot_exprs <- enexprs(...) # defuse before list(...)
  tracks <- as_tracks(list(...), dot_exprs, track_ids(x))
  sublinks <- map(tracks, cluster2sublinks, x$feats[[pid]])
  x <- add_sublink_tracks(x, {{.track_id}}, sublinks, "none")

  # this is just q&d - only adds the ids of the first cluster track. Not sure,
  # how to handle adding multiple ones
  x$feats[[pid]] <- left_join(x$feats[[pid]], tracks[[1]])
  x
}

cluster2sublinks <- function(x, parent_track){
  x <- filter(x, feat_id %in% parent_track$feat_id)
  x %>% split_by(cluster_id) %>%
    keep(~nrow(.) > 1) %>%
    map_df(.id = "cluster_id", function(g){
      mat <- combn(g$feat_id, 2, simplify=TRUE)
      tibble(feat_id = mat[1,], feat_id2 = mat[2,])
    })
}
