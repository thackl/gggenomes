#' Compute a layout for feature data
#'
#' Read feature data such as genes into a tidy dataframe and augment it with
#' layout information based on a sequence layout.
#'
#' Obligatory columns are `seq_id`, `start` and `end`. Also recognized are
#' `strand` and `bin_id`.
#'
#' @param x feature data convertible to a feature layout
#' @param seqs the sequence layout the feature map onto.
#' @param everything set to FALSE to drop optional columns
#' @param ... passed on to `layout_seqs()`
#' @return a tbl_df with plot coordinates
#' @export
as_features <- function(x, seqs, ..., everything=TRUE){
  UseMethod("as_features")
}

#' @export
as_features.default <- function(x, seqs, ..., everything=TRUE) {
  # try to coerce into tbl
  as_features(as_tibble(x), ...)
}

#' @export
as_features.tbl_df <- function(x, seqs, ..., everything=TRUE){
  vars <- c("seq_id","start","end")
  require_vars(x, vars)
  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- as_tibble(select(x, vars, other_vars()))

  TODO("mutate_at - if at all")
  x %<>% mutate_if(is.factor, as.character)
  if(!has_name(x, "strand")){
    x$feature_strand <- 0L
  }else{
    x$feature_strand <- as_numeric_strand(x$strand)
  }
  layout_features(x, seqs, ...)
}

#' Layout features
#'
#' Augment features with all data necessary for plotting
#'
#' @inheritParams as_features
#' @param ... not used
layout.tbl_feature <- function(x, contigs, ...){
  drop_layout(x) %>%
    layout_features(contigs, ...)
}

layout_features <- function(x, seqs, ...){
  join_by <- c("seq_id")
  if(has_name(x, "bin_id")) join_by <- c("seq_id", "bin_id")
  x <- seqs %>% ungroup() %>%
    transmute(seq_id, bin_id, y, .seq_length=length, .seq_strand=strand,
           .seq_offset = pmin(x,xend)) %>%
    inner_join(x, ., by=join_by) %>%
    mutate(
      x = x(start, end, feature_strand, .seq_strand, .seq_offset, .seq_length),
      xend = xend(start, end, feature_strand, .seq_strand, .seq_offset, .seq_length),
      strand = feature_strand * .seq_strand
    ) %>%
    select(y, x, xend, strand, bin_id, everything(),
           -.seq_strand, -.seq_offset, -.seq_length)
}

#' @export
drop_layout.tbl_feature <- function(x, keep="feature_strand"){
  drop <- c("y","x","xend","strand", grep("^\\.", names(x), value=T))
  drop <- drop[!drop %in% keep]
  discard(x, names(x) %in% drop)
}
