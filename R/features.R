#' Compute a layout for feature data
#'
#' Read feature data such as genes into a tidy dataframe and augment it with
#' layout information based on a sequence layout.
#'
#' Obligatory columns are `seq_id`, `start` and `end`. Also recognized are
#' `strand` and `bin_id`.
#'
#' Note `start` and `end` for every record will be coerced so that `start <
#' end`. If no `strand` was provided, `strand` will added and set to "+" for
#' records that initially had `start < end` and "-" for `end < start` inputs. If
#' `strand` was provided, `start` and `end` will be ordered without any
#' additional effect.
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

  # coerce IDs to chars, so we don't get errors in join by mismatched types
  x <- mutate_at(x, vars(seq_id), as.character)

  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- as_tibble(select(x, vars, other_vars()))

  TODO("mutate_at - if at all")
  x %<>% mutate_if(is.factor, as.character)
  if(!has_name(x, "strand")){
    x$strand <- strand_chr(x$start < x$end)
  }else{
    x$strand <- strand_chr(x$strand)
  }
  if(!has_name(x, "feature_id")) x$feature_id <- paste0("f", seq_len(nrow((x))))

  x %<>% swap_if(start > end, start, end)

  layout_features(x, seqs, ...)
}

#' Layout features
#'
#' Augment features with all data necessary for plotting
#'
#' @inheritParams as_features
#' @param marginal how to handle features that stick out of sequences, for
#' example after focusing in on a subregion. Choices are to "drop" them, "keep"
#' them or "trim" them to the subregion boundaries.
#' @param ... not used
layout_features <- function(x, seqs, keep="strand",
  marginal=c("trim", "drop", "keep"), ...){
  marginal <- match.arg(marginal)

  # get rid of old layout
  x <- drop_feature_layout(x, keep)

  # get layout vars necessary for projecting features from seqs
  x <- add_feature_layout_scaffold(x, seqs)

  # ignore features outside subseqs
  x <- trim_features_to_subseqs(x, marginal)

  # project features onto new layout and clean up aux vars (.seq)
  x <- project_features(x) %>%
    select(y, x, xend, bin_id, everything(), -starts_with(".seq"))
  x
}

#' @export
drop_feature_layout <- function(x, seqs, keep="strand"){
  drop <- c("y","x","xend","strand", grep("^\\.", names(x), value=T))
  drop <- drop[!drop %in% keep]
  discard(x, names(x) %in% drop)
}

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
add_features.gggenomes_layout <- function(x, ...){
  if(!has_dots()) return(x)
  dot_exprs <- enexprs(...) # defuse before list(...)
  tracks <- as_tracks(list(...), dot_exprs, track_ids(x))
  add_feature_tracks(x, tracks)
}

add_feature_tracks <- function(x, tracks){
  x$features <- c(x$features, map(tracks, as_features, x$seqs))
  x
}

add_feature_layout_scaffold <- function(x, seqs){
  scaffold <- seqs %>% ungroup() %>% select(
    seq_id, bin_id, y, .seq_strand=strand, .seq_x=x, .seq_start=start, .seq_end=end)

  inner_join(x, scaffold, by=shared_names(x, "seq_id", "bin_id"))
}

trim_features_to_subseqs <- function(x, marginal){
  if(marginal == "drop"){
    x <- mutate(x, .marginal = FALSE)
  }else{
    x <- mutate(x, .marginal = is_marginal(start, end, .seq_start, .seq_end))
  }

  if(marginal == "trim"){
    x %<>% mutate(
      start = ifelse(.marginal & start < .seq_start, .seq_start, start),
      end = ifelse(.marginal & end > .seq_end, .seq_end, end))
  }  # marginals are now also fully contained

  filter(x, .seq_start <= start & end <= .seq_end | .marginal)
}

project_features <- function(x){
  mutate(x,
    x = x(start, end, strand, .seq_x, .seq_start, .seq_strand),
    xend = xend(start, end, strand, .seq_x, .seq_start, .seq_strand))
}

is_marginal <- function(start, end, seq_start, seq_end, closed=FALSE){
  in_range(seq_start, start, end, closed=FALSE) |
    in_range(seq_end, start, end, closed=FALSE)
}
