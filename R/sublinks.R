#' Compute a layout for links linking features
#'
#' Reads sublinks connecting features such as all-vs-all protein blasts into a
#' tidy dataframe. sublinks need to be associated with an already added feature
#' track. The sublinks are internally converted into a regular link track by
#' mapping the feature-based `start` and `end` coordinates to coordinates
#' relative to the sequences underlying the linked features.
#'
#' The only obligatory columns are `feature_id1` & `feature_id2`. Also
#' recognized are `start1/end1`, `start2/end2` and `strand`.
#'
#' Note `start` and `end` for every record will be coerced so that `start <
#' end`. If no `strand` was provided, `strand` will be added and set to "+" for
#' records that initially had `start1 < end1 == start2 < end2` and "-"
#' otherwise. If `strand` was provided, `start` and `end` will be reorganized to
#' conform with `start < end` without any additional effect.
#'
#' @param x sublink data convertible to a link layout
#' @param seqs the sequence layout the linked features map onto.
#' @param features the features the sublinks map onto.
#' @param everything set to FALSE to drop optional columns
#' @param ... passed on to `layout_seqs()`
#' @param transform use if features and sublinks are in different coordinate
#' spaces, i.e. if matching nucleotide-level annotations to protein level
#' annotations, e.g. genes and protein blast results.
#' @return a tbl_df with plot coordinates
#' @export
as_sublinks <- function(x, seqs, features, ..., everything=TRUE){
  UseMethod("as_sublinks")
}

#' @export
as_sublinks.default <- function(x, seqs, features, ..., everything=TRUE) {
  # try to coerce into tbl
  as_sublinks(as_tibble(x), ...)
}

#' @export
as_sublinks.tbl_df <- function(x, seqs, features, ..., everything=TRUE,
    transform = c("none", "aa2nuc", "nuc2aa")){
  transform <- match.arg(transform)
  # TODO - bad transform, not none,aa2nuc,nuc2aa

  vars <- c("feature_id1","feature_id2")
  require_vars(x, vars)

  # coerce IDs to chars, so we don't get errors in join by mismatched types
  x <- mutate_at(x, vars(feature_id1, feature_id2), as.character)
  if(!has_vars(x, c("start1", "end1", "start2", "end2"))){
    if(has_vars(x, c("start1", "end1", "start2", "end2"),any=TRUE)){
      abort("Need either all of start1,fend1,start2,end2 or none!")
    }

    x <- x %>%
      left_join(select(features, feature_id1=feature_id, seq_id1=seq_id, .feat_start=start,
        .feat_end = end, .feat_strand1 = strand), by = shared_names(x, "seq_id1", "feature_id1")) %>%
      mutate(
        start1 = .feat_start, end1 = .feat_end,
        .feat_start=NULL, .feat_end=NULL) %>%
      left_join(select(features, feature_id2=feature_id, seq_id2=seq_id, .feat_start=start,
        .feat_end = end, .feat_strand2 = strand), by = shared_names(x, "seq_id2", "feature_id2")) %>%
      mutate(
        start2 = .feat_start, end2 = .feat_end,
        strand = strand_chr(.feat_strand1 == .feat_strand2),
        .feat_start=NULL, .feat_end=NULL, .feat_strand1=NULL, .feat_strand2=NULL)

    vars <- c("feature_id1", "start1", "end1", "feature_id2", "start2", "end2")
    other_vars <- if(everything) tidyselect::everything else function() NULL;
    x <- as_tibble(select(x, vars, other_vars()))

  }else{
    vars <- c("feature_id1", "start1", "end1", "feature_id2", "start2", "end2")
    other_vars <- if(everything) tidyselect::everything else function() NULL;
    x <- as_tibble(select(x, vars, other_vars()))

    x %<>% mutate_if(is.factor, as.character)
    if(!has_name(x, "strand")){
      x$strand <- strand_chr((x$start1 < x$end1) == (x$start2 < x$end2))
    }else{
      x$strand <- strand_chr(x$strand)
    }

    x <- x %>% swap_if(start1 > end1, start1, end1)
    x <- x %>% swap_if(start2 > end2, start2, end2)

    if(transform == "aa2nuc"){
      x <- mutate(x, start1 = 3*start1-2, end1 = 3*end1-2) %>%
        mutate(start2 = 3*start2-2, end2 = 3*end2-2)
    }else if(transform == "nuc2aa"){
      x <- mutate(x, start1 = (start1+2)/3, end1 = (end1+2)/3) %>%
        mutate(start2 = (start2+2)/3, end2 = (end2+2)/3)
    }

    x <- x %>%
      left_join(select(features, feature_id1=feature_id, seq_id, .feat_start=start,
        .feat_end = end, .feat_strand = strand), by = shared_names(x, "seq_id", "bin_id", "feature_id")) %>%
      mutate(
        start1 = ifelse(is_reverse(.feat_strand), .feat_end-start1, .feat_start+start1),
        end1 = ifelse(is_reverse(.feat_strand), .feat_end-end1, .feat_start+end1),
        .feat_start=NULL, .feat_end=NULL, .feat_strand=NULL) %>%
      left_join(select(features, feature_id2=feature_id, seq_id, .feat_start=start,
        .feat_end = end, .feat_strand = strand), by = shared_names(x, "seq_id", "bin_id", "feature_id")) %>%
      mutate(
        start2 = ifelse(is_reverse(.feat_strand), .feat_end-start2, .feat_start+start2),
        end2 = ifelse(is_reverse(.feat_strand), .feat_end-end2, .feat_start+end2),
        .feat_start=NULL, .feat_end=NULL, .feat_strand=NULL)
  }

  layout_links(x, seqs, ...)
}


#' Add sublinks
#'
#' Add sublinks
#'
#' @param parent_track_id track_id of the features the sublinks map onto.
#' @param ... sublink tables with names, i.e. blast=blast_df, domains=domain_df
#' @inheritParams as_sublinks
#' @param .dots superceed dots with a list of arguments.
#' @export
#' @examples
#' gggenomes %>%
#'   add_sublinks(genes, blastp_hits, transform="aa2nuc")
add_sublinks <- function(x, parent_track_id, ..., transform = "none"){
  UseMethod("add_sublinks")
}

#' @export
add_sublinks.gggenomes <- function(x, parent_track_id, ..., transform = "none"){
  x$data <- add_sublinks(x$data, parent_track_id = {{ parent_track_id }}, ...)
  x
}

#' @export
add_sublinks.gggenomes_layout <- function(x, parent_track_id, ...,
  transform = "none"){

  if(!has_dots()) return(x)
  dot_exprs <- enexprs(...) # defuse before list(...)
  tracks <- as_tracks(list(...), dot_exprs, track_ids(x))
  add_sublink_tracks(x, {{parent_track_id}}, tracks, transform)
}

add_sublink_tracks <- function(x, parent_track_id, tracks, transform){
  features <- pull_track(x, {{parent_track_id}})
  links <- map(tracks, as_sublinks, x$seqs, features, transform = transform)
  x$links <- c(x$links, links)
  x$orig_links <- c(x$orig_links, links)
  x
}

#' Add gene clusters
#' @export
add_clusters <- function(x, parent_track_id, ...){
  UseMethod("add_clusters")
}

#' @export
add_clusters.gggenomes <- function(x, parent_track_id, ...){
  x$data <- add_clusters(x$data, parent_track_id = {{ parent_track_id }}, ...)
  x
}

#' @export
add_clusters.gggenomes_layout <- function(x, parent_track_id, ...){
  if(!has_dots())
    rlang::abort("no clusters data provided - did you forget parent_track_id as first argument")

  pid <- tidyselect::vars_pull(track_ids(x), {{parent_track_id}})
  dot_exprs <- enexprs(...) # defuse before list(...)
  tracks <- as_tracks(list(...), dot_exprs, track_ids(x))
  sublinks <- map(tracks, cluster2sublinks, x$features[[pid]])
  x <- add_sublink_tracks(x, {{parent_track_id}}, sublinks, "none")

  # this is just q&d - only adds the ids of the first cluster track. Not sure,
  # how to handle adding multiple ones
  x$features[[pid]] <- left_join(x$features[[pid]], tracks[[1]])
  x
}

cluster2sublinks <- function(x, parent_track){
  x <- filter(x, feature_id %in% parent_track$feature_id)
  x %>% split_by(cluster_id) %>%
    keep(~nrow(.) > 1) %>%
    map_df(.id = "cluster_id", function(g){
      mat <- combn(g$feature_id, 2, simplify=TRUE)
      tibble(feature_id1 = mat[1,], feature_id2 = mat[2,])
    })
}
