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

  # get new layout vars from seqs
  layout <- seqs %>% ungroup() %>% select(
    seq_id, bin_id, y, .seq_strand=strand, .seq_x=x, .seq_start=start, .seq_end=end)

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
    xend = xend(start, end, strand, .seq_x, .seq_start, .seq_strand)
  ) %>%
    select(y, x, xend, bin_id, everything(),
           -.seq_strand, -.seq_length)
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
add_features.gggenomes_layout <- function(x, ..., .auto_prefix="features"){
  tracks <- list(...)
  names(tracks) <- check_track_ids(names(tracks), track_ids(x), "features",
                                      .auto_prefix)
  # convert to feature layouts
  x$features <- c(x$features, map(tracks, as_features, x$seqs))
  x
}
