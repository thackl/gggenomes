#' Compute a layout for feat data
#'
#' Read feat data such as genes into a tidy dataframe and augment it with
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
#' @param x feat data convertible to a feat layout
#' @param seqs the sequence layout the feat map onto.
#' @param everything set to FALSE to drop optional columns
#' @param ... passed on to `layout_seqs()`
#' @return a tbl_df with plot coordinates
#' @export
#' @keywords internal
as_feats <- function(x, seqs, ..., everything=TRUE){
  UseMethod("as_feats")
}

#' @export
as_feats.default <- function(x, seqs, ..., everything=TRUE) {
  # try to coerce into tbl
  as_feats(as_tibble(x), seqs, ..., everything=everything)
}

#' @export
as_feats.tbl_df <- function(x, seqs, ..., everything=TRUE){
  vars <- c("seq_id","start","end")
  require_vars(x, vars)

  # coerce IDs to chars, so we don't get errors in join by mismatched types
  x <- mutate_at(x, vars("seq_id"), as.character)

  if(!any(seqs$seq_id %in% x$seq_id)){
    warn(paste("No matching seq_ids between feats and seqs.",
         "Maybe you are trying to add subfeats"))
  }

  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- as_tibble(select(x, vars, other_vars()))
  # TODO: mutate_at - if at all
  x %<>% mutate_if(is.factor, as.character)

  # default columns we want to have to make some geoms run smoother
  x <- x %>% introduce(
    feat_id = paste0("f", seq_len(nrow((x)))),
    type = NA,
    strand = x$start < x$end
  ) %>%
  mutate(strand = strand_chr(.data$strand))

  x %<>% swap_if(.data$start > .data$end, .data$start, .data$end)

  layout_feats(x, seqs, ...)
}

#' Layout feats
#'
#' Augment feats with all data necessary for plotting
#'
#' @inheritParams as_feats
#' @param marginal how to handle feats that stick out of sequences, for
#' example after focusing in on a subregion. Choices are to "drop" them, "keep"
#' them or "trim" them to the subregion boundaries.
#' @param ... not used
#' @keywords internal
layout_feats <- function(x, seqs, keep="strand",
  marginal=c("trim", "drop", "keep"), ...){
  marginal <- match.arg(marginal)

  # get rid of old layout
  x <- drop_feat_layout(x, keep)

  # get layout vars necessary for projecting feats from seqs
  x <- add_feat_layout_scaffold(x, seqs)

  # ignore feats outside subseqs
  x <- trim_feats_to_subseqs(x, marginal)

  # project feats onto new layout and clean up aux vars (.seq)
  x <- project_feats(x) %>%
    select(y, x, xend, bin_id, everything(), -starts_with(".seq"))
  x
}

#' Drop feature layout
#'
#' @param x feat_layout
#' @param keep features to keep
#' @export
drop_feat_layout <- function(x, keep="strand"){
  drop <- c("y","x","xend","strand", grep("^\\.", names(x), value=T))
  drop <- drop[!drop %in% keep]
  purrr::discard(x, names(x) %in% drop)
}

#' @describeIn add_tracks Add feature annotations to sequences
#' @order 1
#' @export
#' @examples
#' # Add some repeat annotations
#' gggenomes(seqs=emale_seqs) %>%
#'   add_feats(repeats=emale_tirs) +
#'   geom_seq() + geom_feat()
#' 
add_feats <- function(x, ...){
  UseMethod("add_feats")
}

#' @export
add_feats.gggenomes <- function(x, ...){
  x$data <- add_feats(x$data, ...)
  x
}

#' @export
add_feats.gggenomes_layout <- function(x, ...){
  if(!has_dots()) return(x)
  dot_exprs <- enexprs(...) # defuse before list(...)
  tracks <- as_tracks(list(...), dot_exprs, track_ids(x))
  add_feat_tracks(x, tracks)
}

add_feat_tracks <- function(x, tracks){
  x$feats <- c(x$feats, purrr::map(tracks, as_feats, get_seqs(x)))
  x
}

add_feat_layout_scaffold <- function(x, seqs){
  scaffold <- seqs %>% ungroup() %>% select(
    .data$seq_id, .data$bin_id, .data$y, .seq_strand=.data$strand, .seq_x=.data$x, .seq_start=.data$start, .seq_end=.data$end)

  inner_join(x, scaffold, by=shared_names(x, "seq_id", "bin_id"))
}

trim_feats_to_subseqs <- function(x, marginal){
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

project_feats <- function(x){
  mutate(x,
    x = x(start, end, strand, .seq_x, .seq_start, .seq_strand),
    xend = xend(start, end, strand, .seq_x, .seq_start, .seq_strand))
}

is_marginal <- function(start, end, seq_start, seq_end, closed=FALSE){
  in_range(seq_start, start, end, closed=FALSE) |
    in_range(seq_end, start, end, closed=FALSE)
}

