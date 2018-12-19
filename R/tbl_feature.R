#' Coerce data into a feature tibble
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
  if(!inherits(contigs, "tbl_contig")) stop("expecting contigs as tbl_contig")

  vars <- c("contig_id","start","end")
  require_genome_id <- attr(contigs, "require_genome_id")
  if(!is.null(require_genome_id) && require_genome_id)
    vars <- c("genome_id", vars)
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
  layout_features(x, contigs, ...)
}

#' @export
as_tibble.tbl_feature <- function(x, ...){
  drop_layout(x)
  strip_class(x, "tbl_feature")
}


#' Encode strand numerically
#'
#' This functions converts common formats for strand encoding ("+/-/*",
#' "TRUE/FALSE/NA") into a `(1,-1,0)` representation. This numeric endoding
#' makes it easy to use strandness directly in arthmetic computations.
#'
#' @param strand a vector encoding strandness
#' @export
as_numeric_strand <- function(strand){
  if(is.numeric(strand)){ # make sure it's integer
    strand <- as.integer(strand)
  }else if(rlang::is_logical(strand)){
    strand <- as.integer(strand) * 2 - 1
  }else if(rlang::is_character(strand) || is.factor(strand)){
    if(any(!strand %in%  c("-", "*", "+", NA))){
      bad <- unique(strand[!strand %in%  c("-", "*", "+")])
      stop(paste0("Unknown symbols in strand encoding: ", bad))
    }
    strand <- as.integer(match(strand, c("-", "*", "+")) - 2)
  }else{
    stop("Unknown strand encoding")
  }

  strand[is.na(strand)] <- 0L

  if (min(strand) < -1 || max(strand) > 1){
    stop("Unknown strand encoding")
  }
  strand
}

#' Layout tbl_feature
#'
#' Augment tbl_feature with all data necessary for plotting
#'
#' @inheritParams as_features
#' @param ... not used
layout.tbl_feature <- function(x, contigs, ...){
  drop_layout(x) %>%
    layout_features(contigs, ...)
}

layout_features <- function(x, contigs, ...){
  join_by <- c("contig_id")
  if(has_name(x, "genome_id")) join_by <- c("genome_id", "contig_id")
  x <- contigs %>% ungroup() %>%
    select(genome_id, contig_id, .length=length, y, .strand=strand, .offset) %>%
    inner_join(x, ., by=join_by) %>%
    mutate(
      x = case_when(
        .strand < 0 & feature_strand < 0 ~ .offset+(end-.length)*-1,
        .strand < 0 & feature_strand >=0 ~ .offset+(start-.length)*-1,
        .strand >= 0 & feature_strand < 0 ~ .offset+end,
        .strand >= 0 & feature_strand >= 0 ~ .offset+start
      ),
      xend = case_when(
        .strand < 0 & feature_strand < 0 ~ .offset+(start-.length)*-1,
        .strand < 0 & feature_strand >=0 ~ .offset+(end-.length)*-1,
        .strand >= 0 & feature_strand < 0 ~ .offset+start,
        .strand >= 0 & feature_strand >= 0 ~ .offset+end
      ),
      strand = feature_strand * .strand
      #x =    ifelse(feature_strand < 0, .offset+end, .offset+start),
      #xend = ifelse(feature_strand < 0, .offset+start, .offset+end)
    ) %>%
    select(y, x, xend, strand, genome_id, everything(),
           -.strand, -.length)

  add_class(x, "tbl_feature")
}

#' @export
drop_layout.tbl_feature <- function(x, keep="feature_strand"){
  drop <- c("y","x","xend","strand", grep("^\\.", names(x), value=T))
  drop <- drop[!drop %in% keep]
  discard(x, names(x) %in% drop)
}
