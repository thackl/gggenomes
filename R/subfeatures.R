#' Compute a layout for subfeature data
#'
#' Read subfeature data such as domains or blast hits on genes into a tidy
#' dataframe. Subfeatures need to be associated with an already added feature
#' track. The subfeature track itself is internally converted into a new,
#' regular feature track by mapping the `start` and `end` coordinates provided
#' relative to their parent feature into coordinates relative to the sequences
#' underlying the parent features.
#'
#' Obligatory columns are `feature_id`, `start` and `end`. Also recognized are
#' `strand` and `bin_id`.
#'
#' Note `start` and `end` for every record will be coerced so that `start <
#' end`. If no `strand` was provided, `strand` will be added and set to "+" for
#' records that initially had `start < end` and "-" for `end < start` inputs. If
#' `strand` was provided, `start` and `end` will be reorganized to conform with
#' `start < end` without any additional effect.
#'
#' @param x subfeature data convertible to a feature layout
#' @param seqs the sequence layout the parent features map onto.
#' @param features the parent features the subfeatures map onto.
#' @param everything set to FALSE to drop optional columns
#' @param ... passed on to `layout_seqs()`
#' @param transform use if features and subfeatures are in different coordinate
#' spaces, i.e. if matching nucleotide-level annotations to protein level
#' annotations, e.g. genes and protein blast results.
#' @return a tbl_df with plot coordinates
#' @export
as_subfeatures <- function(x, seqs, features, ..., everything=TRUE){
  UseMethod("as_subfeatures")
}

#' @export
as_subfeatures.default <- function(x, seqs, features, ..., everything=TRUE) {
  # try to coerce into tbl
  as_subfeatures(as_tibble(x), ...)
}

#' @export
as_subfeatures.tbl_df <- function(x, seqs, features, ..., everything=TRUE,
    transform = c("none", "aa2nuc", "nuc2aa")){
  transform <- match.arg(transform)
  # TODO - bad transform, not none,aa2nuc,nuc2aa

  vars <- c("feature_id","start","end")
  require_vars(x, vars)

  # coerce IDs to chars, so we don't get errors in join by mismatched types
  x <- mutate_at(x, vars(feature_id), as.character)

  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- as_tibble(select(x, vars, other_vars()))

  TODO("mutate_at - if at all")
  x %<>% mutate_if(is.factor, as.character)
  if(!has_name(x, "strand")){
    x$strand <- strand_chr(x$start < x$end)
  }else{
    x$strand <- strand_chr(x$strand)
  }

  x <- x %>% swap_if(start > end, start, end)
  if(transform == "aa2nuc") x <- mutate(x, start = 3*start-2, end = 3*end-2)
  if(transform == "nuc2aa") x <- mutate(x, start = (start+2)/3, end = (end+2)/3)

  x <- x %>% left_join(select(features, feature_id, seq_id, .feat_start=start,
      .feat_end = end, .feat_strand = strand), by = shared_names(x, "seq_id", "bin_id", "feature_id")) %>%
    mutate(
      start = ifelse(is_reverse(.feat_strand), .feat_end-start, .feat_start+start),
      end = ifelse(is_reverse(.feat_strand), .feat_end-end, .feat_start+end),
      .feat_start=NULL, .feat_end=NULL, .feat_strand=NULL)

  layout_features(x, seqs, ...)
}


#' Add subfeatures
#' @param parent_track_id
#' @param ... subfeature tables with names, i.e. blast=blast_df, domains=domain_df
#' @inheritParams as_subfeatures
#' @param .dots superceed dots with a list of arguments.
#' @export
#'
#' @examples
#' gggenomes %>%
#'   add_subfeatures(genes, blastp_hits, transform="aa2nuc")
add_subfeatures <- function(x, parent_track_id, ..., transform = "none", .dots=NULL){
  UseMethod("add_subfeatures")
}

#' @export
add_subfeatures.gggenomes <- function(x, parent_track_id, ..., transform = "none", .dots=NULL){
  x$data <- add_subfeatures(x$data, parent_track_id = {{ parent_track_id }}, ...)
  x
}

#' @export
add_subfeatures.gggenomes_layout <- function(x, parent_track_id, ...,
  transform = "none"){

  if(!has_dots()) return(x)
  dot_exprs <- enexprs(...) # defuse before list(...)
  tracks <- as_tracks(list(...), dot_exprs, track_ids(x))
  add_subfeature_tracks(x, {{parent_track_id}}, tracks, transform)
}

add_subfeature_tracks <- function(x, parent_track_id, tracks, transform){
  features <- track(x, {{parent_track_id}})
  x$features <- c(x$features, map(
    tracks, as_subfeatures, x$seqs, features, transform = transform))
  x
}
