#' Use tracks inside and outside `geom_*` calls
#'
#' Track selection works like [dplyr::pull()] and supports unquoted ids and
#' positional arguments. `...` can be used to subset the data in
#' [dplyr::filter()] fashion. `pull`-prefixed variants return the specified
#' track from a gggenome object. Unprefixed variants work inside `geom_*` calls.
#' @param .x A gggenomes or gggenomes_layout object.
#' @param .track_id The track to pull out, either as a literal variable name or
#'   as a positive/negative integer giving the position from the left/right.
#' @param ... Logical predicates passed on to [dplyr::filter]. "seqs", "feats",
#'   "links". Affects position-based selection.
#' @param .ignore track names to ignore when selecting by position.
#' @param .geneify add dummy type, introns and geom_id column to play nicely
#'   with geoms supporting multi-level and spliced gene models.
#' @param .gene_types return only feats of this type (`type %in% .gene_types`)
#' @param .adjacent_only filter for links connecting direct neighbors
#'   (`abs(y-yend)==1)`)
#' @param .group what variables to use in grouping of bins from seqs in addition
#'   to `y` and `bin_id`. Use this to get additional shared variables from the
#'   seqs table into the bins table.
#' @param .track_type restrict to these types of tracks - any combination of
#'   "seqs", "feats", "links".
#' @examples
#'
#' gg <- gggenomes(emale_genes, emale_seqs, emale_tirs, emale_ava)
#' gg %>% track_info()            # info about track ids, positions and types
#'
#' # get first feat track that isn't "genes" (all equivalent)
#' gg %>% pull_feats()            # easiest
#' gg %>% pull_feats(feats)       # by id
#' gg %>% pull_feats(1)           # by position
#' gg %>% pull_feats(2, .ignore=NULL)  # default .ignore="genes"
#'
#' # get "seqs" track (always track #1)
#' gg %>% pull_seqs()
#'
#' # plot integrated transposons and GC content for some viral genomes
#' gg <- gggenomes(seqs=emale_seqs, feats=list(emale_ngaros, GC=emale_gc))
#' gg + geom_seq() +
#'   geom_feat(color="skyblue") + # defaults to data=feats()
#'   geom_line(aes(x, y+score-.6, group=y), data=feats(GC), color="gray60")
#' @describeIn pull_track by default pulls out the first feat track not named
#'   "genes".
#' @export
feats <- function(.track_id=1, ..., .ignore="genes", .geneify=FALSE){
  dots <- quos(...)
  function(.x, ...){
    pull_feats(.x, {{.track_id}}, !!! dots, .ignore=.ignore, .geneify=.geneify)
  }
}
#' @export
feats0 <- function(.track_id=1, ..., .ignore=NA, .geneify=FALSE){
  dots <- quos(...)
  function(.x, ...){
    pull_feats(.x, {{.track_id}}, !!! dots, .ignore=.ignore, .geneify=.geneify)
  }
}

#' @describeIn pull_track pulls out the first feat track (genes), filtering
#' for records with `type=="CDS"`, and adding a dummy `gene_id` column if missing
#' to play nice with multi-exon `geom`s.
#' @export
genes <- function(..., .gene_types=c("CDS", "mRNA", "tRNA", "tmRNA", "ncRNA", "rRNA")){
  dots <- quos(...)
  function(.x, ...){
    pull_genes(.x, !!! dots, .gene_types=.gene_types)
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
pull_feats <- function(.x, .track_id=1, ..., .ignore="genes", .geneify=FALSE){
  UseMethod("pull_feats")
}
#' @export
pull_feats.gggenomes <- function(.x, .track_id=1, ..., .ignore="genes", .geneify=FALSE){
  pull_feats(.x$data, {{.track_id}}, ..., .ignore=.ignore, .geneify=.geneify)
}
#' @export
pull_feats.gggenomes_layout <- function(.x, .track_id=1, ..., .ignore="genes",
    .geneify=FALSE){
  track <- pull_track(.x, {{.track_id}}, ..., .track_type="feats", .ignore=.ignore)
  if(.geneify){
    track <- introduce(track,
      type="CDS", introns = list(NULL),
      geom_id = paste0("geom_", row_number()))
  }
  filter(track, ...)
}

#' @rdname pull_track
#' @export
pull_genes <- function(.x, ..., .gene_types=c("CDS", "mRNA", "tRNA", "tmRNA",
    "ncRNA", "rRNA")){
  UseMethod("pull_genes")
}
#' @export
pull_genes.gggenomes <- function(.x, ..., .gene_types=c("CDS", "mRNA", "tRNA",
    "tmRNA", "ncRNA", "rRNA")){
  pull_genes(.x$data, ..., .gene_types=.gene_types)
}
#' @export
pull_genes.gggenomes_layout <- function(.x, ..., .gene_types=c("CDS", "mRNA",
    "tRNA", "tmRNA", "ncRNA", "rRNA")){
  track <- pull_feats(.x, 1, ..., .ignore=NULL, .geneify=TRUE)
  if(length(.gene_types) > 0) track <- filter(track, type %in% .gene_types)
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
  .group <- c(vars(bin_id), .group)
  get_seqs(.x) %>% filter(...) %>% group_by(!!!.group) %>%
    summarize(y=min(y), x = min(x,xend), xend = max(x,xend)) %>%
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
#' @param ignore names of tracks to ignore when selecting by position.
#' @return The selected track_id as an unnamed string
vars_track <- function(x, track_id, track_type = c("seqs", "feats", "links"),
    ignore = NULL){
  track_type <- match_arg(track_type, several.ok = T)
  track_ids <- track_ids(x, track_type)

  # only ignore in integer position context
  if(!is.null(ignore) && !quo_is_symbolic(enquo(track_id)) &&
     is_integerish(track_id)){
    track_ids <- setdiff(track_ids, ignore)
  }

  if(length(track_ids) < 1){
    abort(c("Track not found", paste("Tracks ignored by position:", comma(ignore))))
  }

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

#' Error messages for vars_track
#' @keywords internal
vars_track_error <- function(bad_value, track_ids, ignore){
  if(is_function(bad_value)) bad_value <- "<function>"
  if(is.numeric(bad_value)) bad_value <- as.character(bad_value)
  c(
    paste("Track", as_name(bad_value), "not found"),
    paste0("Available tracks: ", comma(track_ids), " (", length(track_ids), ")"),
    paste("Tracks ignore by position:", comma(ignore)))
}
