#' @describeIn add_tracks Add gene clusters or other feature groups. Takes a
#' data.frame with at least two required columns `cluster_id` and `feat_id`. The
#' data.frame is converted to a link track connecting features belonging to the
#' same cluster over their entire length. Additionally, the data.frame is joined
#' to the parent feature track, adding `cluster_id` and all additional columns
#' to the parent table.
#' @order 5
#' @export
#' @examples
#' # add clusters
#' gggenomes(emale_genes, emale_seqs) %>%
#'   add_clusters(emale_cogs) %>%
#'   sync() + # works because clusters
#'   geom_link() + # become links
#'   geom_seq() +
#'   # works because cluster info is joined to gene track
#'   geom_gene(aes(fill = ifelse(is.na(cluster_id), NA,
#'     stringr::str_glue("{cluster_id} [{cluster_size}]")
#'   ))) +
#'   scale_fill_discrete("COGs")
#'
add_clusters <- function(x, ..., .track_id = "genes") {
  UseMethod("add_clusters")
}

#' @export
add_clusters.gggenomes <- function(x, ..., .track_id = "genes") {
  x$data <- add_clusters(x$data, ..., .track_id = {{ .track_id }})
  x
}

#' @importFrom rlang .data
#' @export
add_clusters.gggenomes_layout <- function(x, ..., .track_id = "genes") {
  if (length(...) == 0) {
    warn("No clusters data provided - check your arguments")
    return(x)
  }

  pid <- tidyselect::vars_pull(track_ids(x), {{ .track_id }})
  dot_exprs <- enexprs(...) # defuse before list(...)
  tracks <- as_tracks(list(...), dot_exprs, track_ids(x))

  tracks <- purrr::map(tracks, function(track) {
    require_vars(track, c("feat_id", "cluster_id"))

    track <- dplyr::filter(track, .data$feat_id %in% x$feats[[pid]]$feat_id)
    if (nrow(track) < 1) {
      warn(str_glue(
        "No matches between clusters and parent track based on ",
        "`track_id`. Check your IDs and arguments"
      ))
      return(x)
    }

    if (any(duplicated(track$feat_id))) {
      dup_ids <- track$feat_id[duplicated(track$feat_id)][1:5]
      abort(c("Duplicated `feat_id`s not allowed:", str_glue("{dup_ids}")))
    }

    track
  })

  sublinks <- purrr::map(tracks, cluster2sublinks, x$feats[[pid]]) %>%
    purrr::compact() # can be empty tibble of all clusters were singletons
  if (length(sublinks) < length(tracks)) {
    warn("At least one cluster table had only singletons, so no links were produced")
  }
  if (length(sublinks)) {
    x <- add_sublink_tracks(x, {{ .track_id }}, sublinks, transform = "none")
  }

  # this is just q&d - only adds the ids of the first cluster track. Not sure,
  # how to handle adding multiple ones
  if (length(tracks) > 1) {
    warn(str_glue(
      "If adding multiple cluster tables, all are added as ",
      "individual link tracks, but only the first table is joined with the ",
      "parent feat table"
    ))
  }

  x$feats[[pid]] <- left_join(x$feats[[pid]], tracks[[1]])
  x
}

cluster2sublinks <- function(x, parent_track) {
  x %>%
    split_by(.data$cluster_id) %>%
    purrr::keep(~ nrow(.) > 1) %>% # links need >2 members, ignore singletons
    purrr::map_df(.id = "cluster_id", function(g) {
      mat <- utils::combn(g$feat_id, 2, simplify = TRUE)
      tibble(feat_id = mat[1, ], feat_id2 = mat[2, ])
    })
}
