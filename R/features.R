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
#' `strand` was provided, the `start` and `end` will be ordered without any
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
  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- as_tibble(select(x, vars, other_vars()))

  TODO("mutate_at - if at all")
  x %<>% mutate_if(is.factor, as.character)
  if(!has_name(x, "strand")){
    x$strand <- as_numeric_strand(x$start < x$end)
  }else{
    x$strand <- as_numeric_strand(x$strand)
  }

  x %<>% swap(start > end, start, end)

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

  # get new layout vars from seqs
  layout <- seqs %>% ungroup() %>%
    transmute(
      seq_id, bin_id, y, .seq_length=length, .seq_strand=strand,
      .seq_offset = pmin(x,xend)-ifelse(.seq_strand < 0, end, start),
      .seq_x=x, .seq_start=start, .seq_end=end)

  # project features onto new layout
  join_by <- if(has_name(x, "bin_id")){c("seq_id", "bin_id")}else{"seq_id"}
  x <- inner_join(x, layout, by=join_by)

  if(marginal == "drop"){
    # get only all fully contained features
    x %<>% filter(.seq_start <= start & end <= .seq_end)
  }else{
    x %<>% mutate(
      .marginal = in_range(.seq_start, start, end, closed=FALSE) |
        in_range(.seq_end, start, end, closed=FALSE))

    if(marginal == "keep"){
      # get all fully contained and jutting features
      x %<>% filter(.seq_start <= start & end <= .seq_end | .marginal)
    }else if(marginal == "trim"){
      x %<>% mutate(
          start = ifelse(.marginal & start < .seq_start, .seq_start, start),
          end = ifelse(.marginal & end > .seq_end, .seq_end, end)) %>%
        # marginals are now also fully contained
        filter(.seq_start <= start & end <= .seq_end)
    }
  }

  x %>%  mutate(
    x = x(start, end, strand, .seq_x, .seq_start, .seq_strand),
    xend = xend(start, end, strand, .seq_x, .seq_start, .seq_strand),
    display_strand = strand * .seq_strand # this is mostly for convenience to map aes to strand as seen
  ) %>%
    select(y, x, xend, display_strand, bin_id, everything(),
           -.seq_strand, -.seq_offset, -.seq_length)
}

#' @export
drop_feature_layout <- function(x, seqs, keep="strand"){
  drop <- c("y","x","xend","strand", grep("^\\.", names(x), value=T))
  drop <- drop[!drop %in% keep]
  discard(x, names(x) %in% drop)
}
