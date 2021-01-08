#' Use a certain track, possibly filtered
#'
#' Track selection works like [dplyr::pull()] and supports unquoted ids and
#' positional arguments. `...` can be used to subset the data in [dplyr::filter()]
#' fashion. `pull`-prefixed variants return the specified track from a gggenome
#' object. Unprefixed variants work inside `geom_*` calls.
#' @param .x A gggenomes or gggenomes_layout object.
#' @param .track_id The track to pull out, either as a literal variable name or
#'   as a positive/negative integer giving the position from the left/right.
#' @param ... Logical predicates passed on to [dplyr::filter].
#'   "seqs", "features", "links". Affects position-based selection.
#' @param .ignore a regular string vector with track_ids to ignore.
#' @param .geneify add dummy gene_id and type column to play nicely with geoms
#'   supporting multi-exon features.
#' @param .cds_only filter for features tagged as coding sequence in the type column (`type=="CDS"`)
#' @param .adjacent_only filter for links connecting direct neighbors (`abs(y-yend)==1)`)
#' @param .group what variables to use in grouping of bins from seqs in addition
#' to `y` and `bin_id`. Use this to get additional shared variables from the
#' seqs table into the bins table.
#' @param .track_type restrict to these types of tracks - any combination of "seqs", "features", "links".
#' @examples
#'
#' gg <- gggenomes(emale_seqs, emale_genes, emale_tirs, emale_links)
#' gg %>% track_info()            # info about track ids, positions and types
#'
#' # get first feat track that isn't "genes" (all equivalent)
#' gg %>% pull_feats()            # easiest
#' gg %>% pull_feats(features)    # by id
#' gg %>% pull_feats(1)           # by position
#' gg %>% pull_feats(2, .ignore=NULL)  # default .ignore="genes"
#'
#' # get "seqs" track (always track #1)
#' gg %>% pull_seqs()
#'
#' # plot integrated transposons and GC content for some viral genomes
#' gg <- gggenomes(emale_seqs[1:8,],
#'   features=list(emale_transposons, GC=emale_gc))
#' gg + geom_seq() +
#'   geom_feature(color="skyblue") + # defaults to data=feats()
#'   geom_line(aes(x, y+score-.6, group=y), data=feats(GC), color="gray60")
#' @describeIn pull_track by default pulls out the first feature track not named "genes".
#' @export
feats <- function(.track_id=1, ..., .ignore="genes", .geneify=FALSE){
  dots <- quos(...)
  function(.x, ...){
    pull_features(.x, {{.track_id}}, !!! dots, .ignore=.ignore, .geneify=.geneify)
  }
}
#' @describeIn pull_track pulls out the first feature track (genes), filtering
#' for records with `type=="CDS"`, and adding a dummy `gene_id` column if missing
#' to play nice with multi-exon `geom`s.
#' @export
genes <- function(..., .cds_only=TRUE){
  dots <- quos(...)
  function(.x, ...){
    pull_genes(.x, !!! dots, .cds_only=.cds_only)
  }
}
#' @describeIn pull_track by default pulls out the first link track.
#' @export
links <- function(.track_id=1, ..., .ignore=NULL, .adjacent_only=TRUE){
  dots <- quos(...)
  function(.x, ...){
    pull_links(.x, {{.track_id}}, !!! dots, .ignore=.ignore, .adjacent_only=.adjacent_only)
  }
}
#' @describeIn pull_track pulls out the seqs track (there is only one).
#' @export
seqs <- function(...){
  dots <- quos(...)
  function(.x, ...){
    pull_seqs(.x, !!! dots)
  }
}
#' @describeIn pull_track pulls out a binwise summary table of the seqs data powering
#'   `geom_bin_*()` calls. The bin table is not a real track, but recomputed
#'   on-the-fly.
#' @export
bins <- function(..., .group=vars()){
  dots <- quos(...)
  function(.x, ...){
    pull_bins(.x, !!! dots, .group=.group)
  }
}
#' @describeIn pull_track pulls from all tracks in order seqs, feats, links.
#' @export
track <- function(.track_id=1, ..., .track_type=NULL, .ignore=NULL){
  dots <- quos(...)
  function(.x, ...){
    pull_track(.x, {{.track_id}}, !!! dots, .track_type=.track_type, .ignore=.ignore)
  }
}

#' @rdname pull_track
#' @export
pull_features <- function(.x, .track_id=1, ..., .ignore="genes", .geneify=FALSE){
  UseMethod("pull_features")
}
#' @export
pull_features.gggenomes <- function(.x, .track_id=1, ..., .ignore="genes", .geneify=FALSE){
  pull_features(.x$data, {{.track_id}}, ..., .ignore=.ignore, .geneify=.geneify)
}
#' @export
pull_features.gggenomes_layout <- function(.x, .track_id=1, ..., .ignore="genes",
    .geneify=FALSE){
  track <- pull_track(.x, {{.track_id}}, ..., .track_type="features", .ignore=.ignore)
  if(.geneify){
    if(!has_name(track, "type")) track[["type"]] <- "CDS"
    if(!has_name(track, "gene_id"))
      track[["gene_id"]] <- paste0("__", seq_len(nrow((track))))
  }
  filter(track, ...)
}

#' @rdname pull_track
#' @export
pull_genes <- function(.x, ..., .cds_only=TRUE){
  UseMethod("pull_genes")
}
#' @export
pull_genes.gggenomes <- function(.x, ..., .cds_only=TRUE){
  pull_genes(.x$data, ..., .cds_only=.cds_only)
}
#' @export
pull_genes.gggenomes_layout <- function(.x, ..., .cds_only=TRUE){
  track <- pull_features(.x, 1, ..., .ignore=NULL, .geneify=TRUE)
  if(.cds_only) track <- filter(track, type=="CDS")
  track
}

#' @rdname pull_track
#' @export
pull_links <- function(.x, .track_id=1, ..., .ignore=NULL, .adjacent_only=TRUE){
  UseMethod("pull_links")
}
#' @export
pull_links.gggenomes <- function(.x, .track_id=1, ..., .ignore=NULL, .adjacent_only=TRUE){
  pull_links(.x$data, {{.track_id}}, ..., .ignore=.ignore, .adjacent_only=.adjacent_only)
}
#' @export
pull_links.gggenomes_layout <- function(.x, .track_id=1, ..., .ignore=NULL, .adjacent_only=TRUE){
  track <- pull_track(.x, {{.track_id}}, ..., .track_type="links", .ignore=.ignore)
  if(.adjacent_only) track <- filter(track, abs(y-yend)==1)
  track
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
  filter(get_seqs(.x), ...)
}

#' @rdname pull_track
#' @export
pull_bins <- function(.x, ..., .group=vars()){
  UseMethod("pull_bins")
}
#' @export
pull_bins.gggenomes <- function(.x, ..., .group=vars()){
  pull_bins(.x$data, ..., .group=.group)
}
#' @rdname pull_track
#' @export
pull_bins.gggenomes_layout <- function(.x, ..., .group=vars()){
  .group <- c(vars(y, bin_id), .group)
  get_seqs(.x) %>% filter(...) %>% group_by(!!!.group) %>%
    summarize(x = min(x,xend), xend = max(x,xend)) %>%
    ungroup()
}

#' @rdname pull_track
#' @export
pull_track <- function(.x, .track_id=1, ..., .track_type=NULL, .ignore=NULL){
  UseMethod("pull_track")
}
#' @export
pull_track.gggenomes <- function(.x, .track_id=1, ..., .track_type=NULL, .ignore=NULL){
  pull_track(.x$data, {{.track_id}}, ..., .track_type=.track_type, .ignore=.ignore)
}
#' @export
pull_track.gggenomes_layout <- function(.x, .track_id=1, ..., .track_type=NULL, .ignore=NULL){
  track_id <- vars_track(.x, {{.track_id}}, track_type=.track_type, ignore=.ignore)
  filter(.x[[track_type(.x, track_id)]][[track_id]], ...)
}

#' Tidyselect track variables
#'
#' Based on `tidyselect::vars_pull`. Powers track selection in `pull_track()`.
#' Catches and modifies errors from vars_pull to track-relevant info.
#' @param x A gggenomes or gggenomes_layout object
#' @param track_id a quoted or unquoted name or as positive/negative integer
#'   giving the position from the left/right.
#' @param track_type restrict to these types of tracks - affects position-based
#'   selection
#' @return The selected track_id as an unnamed string
vars_track <- function(x, track_id, track_type = c("seqs", "features", "links"),
    ignore = NULL){
  track_type <- match_arg(track_type, several.ok = T)
  track_ids <- track_ids(x, track_type)
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
