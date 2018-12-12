#' Coerce data into feature tibble.
#' 
#' @param x an object coercible to a `tbl_feature` 
#' @param contigs a tbl_contig the features are associated with
#' @param ... passed on to `layout()`
#' @param everything keep non-required columns
#' @export
as_features <- function(x, contigs, ..., everything=TRUE){
  UseMethod("as_features")
}

#' @export
as_features.default <- function(x, contigs, ..., everything=TRUE) {
  # try to coerce into tbl
  as_features(as_tibble(x), ...)
}

#' @export
as_features.tbl_df <- function(x, contigs, ..., everything=TRUE){
  # TODO: require genome_id if duplicated(contigs$contig_id)
  if(!inherits(contigs, "tbl_contig")) stop("expecting contigs as tbl_contig")
  vars <- c("contig_id","start","end")
  require_vars(x, vars)
  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- as_tibble(select(x, vars, other_vars()))

  # TODO: mutate_at - if at all
  x %<>% mutate_if(is.factor, as.character)

  if(!has_vars(x, "strand")){
    x$.strand <- 0L
  }else{
    if(is_integer(x$strand)){
      x$.strand <- x$strand
    }else if(is_double(x$strand)){
      x %<>% mutate(.strand = as.integer(strand))
    }else if(rlang::is_character(x$strand)){
      x$.strand <- as.integer(match(x$strand, c("-", "*", "+")) - 2)
    }else{
      stop("Unknown strand encoding")
    }
  }

  layout(set_class(x, "tbl_feature", "prepend"), contigs, ...)
}

# backtransform (drop for features, features, backtrans for links
#' @export
as_tibble.tbl_feature <- function(x, ...){
  stop("TODO")
}

#' Layout tbl_feature
#'
#' Augment tbl_feature with all data necessary for plotting
#' 
#' @inheritParams as_features
#' @param ... not used
layout.tbl_feature <- function(x, contigs, ...){
  x <- contigs %>% ungroup() %>%
    select(genome_id, contig_id, .y, .offset, .gcstrand=.strand) %>%
    inner_join(x, .) %>%
    mutate(
      .strand = .strand*.gcstrand,
      .x =    dplyr::if_else(.strand < 0, .offset+end, .offset+start),
      .xend = dplyr::if_else(.strand < 0, .offset+start, .offset+end)
    ) %>%
      select(.y, .x, .xend, .strand, genome_id, everything())

  set_class(x, "tbl_feature", "prepend")
}
